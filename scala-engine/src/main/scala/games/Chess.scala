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
  } else if (toPiece == "WS" || toPiece == "BS")
    return true
  else if (!toPiece_isBlack ^ black)
    return false
  true
}

def pawn(black: Boolean)(gameState: GameState, move: ((Int, Int),(Int, Int))): Boolean = {
  val (from, to) = move
  (math.abs(to._1 - from._1) + math.abs(to._2 - from._2) <= 2 && math.abs(to._1 - from._1) > 0) && (!black ^ to._1 > from._1) && (math.abs(to._1 - from._1) != 2 || ((from._1 == 1  && black) || (from._1 == 6 && !black))) && (to._2 == from._2 ^ ((gameState._1(to._1)(to._2) match {
    case "WS" | "BS" => false
    case _ => true
  }) && (black ^ piece_isBlack(gameState._1(to._1)(to._2)))))
}

def knight(black: Boolean)(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (math.abs(to._1 - from._1) + math.abs(to._2 - from._2) != 3 || math.abs(to._1 - from._1) == 0)
    return false
  true
}

def bishop(black: Boolean)(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
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

def rook(black: Boolean)(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
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

def blank(black: Boolean = false)(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  false
}

def queen(black: Boolean)(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  rook(black)(gameState, move) || bishop(black)(gameState, move)
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
    case "WN" => knight(black = false)
    case "WB" => bishop(black = false)
    case "WK" => king(black = false)
    case "WQ" => queen(black = false)
    case "WR" => rook(black = false)
    case "BP" => pawn(black = true)
    case "BN" => knight(black = true)
    case "BB" => bishop(black = true)
    case "BK" => king(black = true)
    case "BQ" => queen(black = true)
    case "BR" => rook(black = true)
    case _ => blank()
  }
  if(!piece(gameState, (from, to)))
    return (gameState, false)
  if (!generalChecks(piece_isBlack(gameState._1(from._1)(from._2)), gameState, (from, to)))
    return (gameState, false)
  val newState: Array[Array[String]] = gameState._1.map(row => row.map(identity))
  assert(newState.length == 8 && newState(0).length == 8)
  newState(to._1)(to._2) = gameState._1(from._1)(from._2)
  newState(from._1)(from._2) = if((from._1 + from._2) % 2 == 0) "WS" else "BS"
  ((newState, 3 - gameState._2), true)
}

def pieces = Map(
  "WR" -> " \u2656.",
  "WN" -> " \u2658",
  "WB" -> " \u2657",
  "WQ" -> " \u2655",
  "WK" -> " \u2654",
  "WP" -> " \u2659",
  "BR" -> " \u265C",
  "BN" -> " \u265E",
  "BB" -> " \u265D",
  "BQ" -> " \u265B",
  "BK" -> " \u265A",
  "BP" -> "♟ ",
  "BS" -> "   ",
  "WS" -> "   "
)

def chessDrawer(gameState: GameState): Unit = {
  println(Console.RED + "Player " + gameState._2 + "'s Turn:")
  for(row <- 0 to 7) {
    print(Console.RED + (8 - row) + "\t" + Console.RESET)
    for(col <- 0 to 7) {
      print((if((row + col) % 2 == 0) "\u001b[47m" else "\u001b[40m") + pieces(gameState._1(row)(col)) + "\u001b[0m")
    }
    println()
  }
  print(" \t")
  for(col <- 0 to 7) {
    print(Console.RED + ('A' + col).toChar + "  ")
  }
  println(Console.RESET)
}
