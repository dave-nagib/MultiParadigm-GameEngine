package org.gengine
package games
import scala.util.matching.Regex

def piece_isBlack(piece: String): Boolean = {
  piece(0) == 'B'
}

def generalChecks(black: Boolean, gameState: GameState, move: ((Int, Int),(Int, Int))): Boolean = {
  val (from, to) = move
  val toPiece = gameState._1(to._1)(to._2)
  val toPiece_isBlack: Boolean = piece_isBlack(toPiece)
  if(black ^ (gameState._2 % 2 == 0)) {
    return false
  } else if (toPiece == " ")
    return true
  else if (!toPiece_isBlack ^ black)
    return false
  true
}

def pawn(black: Boolean)(gameState: GameState, move: ((Int, Int),(Int, Int))): Boolean = {
  val (from, to) = move
  (math.abs(to._1 - from._1) + math.abs(to._2 - from._2) <= 2 && math.abs(to._1 - from._1) > 0) && (!black ^ to._1 > from._1) && (math.abs(to._1 - from._1) != 2 || ((from._1 == 1  && black) || (from._1 == 6 && !black))) && (to._2 == from._2 ^ ((gameState._1(to._1)(to._2) match {
    case " " => false
    case _ => true
  }) && (black ^ piece_isBlack(gameState._1(to._1)(to._2)))))
}

def knight(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (math.abs(to._1 - from._1) + math.abs(to._2 - from._2) != 3 || math.abs(to._1 - from._1) == 0)
    return false
  true
}

def bishop(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (math.abs(to._1 - from._1) != math.abs(to._2 - from._2))
    return false

  val rowCoef: Int =
    if(from._1 > to._1)
      -1
    else
      1

  val colCoef: Int =
    if (from._2 > to._2)
      -1
    else
      1

  for(i <- 1 until math.abs(to._1 - from._1))
    if(gameState._1(from._1 + i * rowCoef)(from._2 + i * colCoef) != "WS" && gameState._1(from._1)(from._2) != "BS")
      return false

  true
}

def rook(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (math.abs(to._1 - from._1) != 0 && math.abs(to._2 - from._2) != 0)
    return false

  val rowCoef: Int =
    if (from._1 == to._1)
      0
    else if(from._1 > to._1)
      -1
    else
      1

  val colCoef: Int =
    if (from._2 == to._2)
      0
    else if (from._2 > to._2)
      -1
    else
      1

  for (i <- 1 until math.abs(to._1 - from._1) + math.abs(to._2 - from._2))
    if (gameState._1(from._1 + i * rowCoef)(from._2 + i * colCoef) != "WS" && gameState._1(from._1)(from._2) != "BS")
      return false

  true
}

def blank(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  false
}

def queen(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  rook(gameState, move) || bishop(gameState, move)
}

def king(black: Boolean)(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (!(math.abs(to._1 - from._1) + math.abs(to._2 - from._2) == 2 && math.abs(to._1 - from._1) > 0))
    return false
  true
}


def translate_input(move: Array[String]): ((Int, Int), (Int, Int)) = {
  val translated_input: Array[IndexedSeq[Int]] = new Array[IndexedSeq[Int]](2)
  for (i <- move.indices)
    translated_input(i) = move(i).map(c => {
      if (c.isLetter)
        c.toLower - 'a'
      else
        56 - c
    })
  (
    (translated_input(0)(1), translated_input(0)(0)),
    (translated_input(1)(1), translated_input(1)(0))
  )
}

def chessController(gameState: GameState, move: String): (GameState, Boolean) = {
  val (from, to) = translate_input({
    val moves = (new Regex("[a-hA-H]\\d") findAllIn move).take(2).toArray
    if (moves.length != 2)
      return (gameState, false)
    else
      moves
  })
  val piece: (GameState, ((Int, Int), (Int, Int))) => Boolean = gameState._1(from._1)(from._2) match {
    case "WP" => pawn(black = false)
    case "WN" => knight
    case "WB" => bishop
    case "WK" => king(black = false)
    case "WQ" => queen
    case "WR" => rook
    case "BP" => pawn(black = true)
    case "BN" => knight
    case "BB" => bishop
    case "BK" => king(black = true)
    case "BQ" => queen
    case "BR" => rook
    case _ => blank
  }
  if(!piece(gameState, (from, to)))
    return (gameState, false)
  if (!generalChecks(piece_isBlack(gameState._1(from._1)(from._2)), gameState, (from, to)))
    return (gameState, false)
  val newState: Array[Array[String]] = gameState._1.map(row => row.map(identity))
  assert(newState.length == 8 && newState(0).length == 8)
  newState(to._1)(to._2) = gameState._1(from._1)(from._2)
  newState(from._1)(from._2) = " "
  ((newState, 3 - gameState._2), true)
}

def pieces(piece: String) = piece match{
  case "WR" => " \u265C "
  case "WN" => " \u265E "
  case "WB" => " \u265D "
  case "WQ" => " \u265B "
  case "WK" => " \u265A "
  case "WP" => " \u2659 "
  case "BR" => s" ${Console.BLACK}\u265C "
  case "BN" => s" ${Console.BLACK}\u265E "
  case "BB" => s" ${Console.BLACK}\u265D "
  case "BQ" => s" ${Console.BLACK}\u265B "
  case "BK" => s" ${Console.BLACK}\u265A "
  case "BP" => s" ${Console.BLACK}\u2659 "
  case " " => s" ${Console.GREEN}\u265B "
  case x: String => x
}

def chessDrawer(gameState: GameState): Unit = {
  println(Console.RED + "Player " + gameState._2 + "'s Turn:")
  for(row <- gameState._1.indices) {
    print(Console.RED + (8 - row) + "\t" + Console.RESET)
    for(col <- gameState._1.indices) {
      print((if((row + col) % 2 == 0) "\u001b[41;2m" else "\u001b[42m") + pieces(gameState._1(row)(col)) + "\u001b[0m")
    }
    println()
  }
  print(" \t ")
  for(col <- gameState._1.indices) {
    print(Console.RED + ('A' + col).toChar + "   ")
  }
  println(Console.RESET)
}
