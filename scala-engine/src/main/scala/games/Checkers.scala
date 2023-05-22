package org.gengine
package games
import scala.annotation.tailrec
import scala.util.matching.Regex

/* ------------------------------------------------ UTILITY FUNCTIONS ------------------------------------------------ */

// Checks whether a square is white using the fact that white squares' indices have an even sum.
def isWhite(x: Int, y: Int) : Boolean = {
  (x+y) % 2 == 0
}

// Validates that the player has a piece at the specific location, and checks if a piece is a pawn or not.
// Player 1 might have piece 1 (Pawn) or 3 (King), player 2 might have piece 2 (Pawn) or 4 (King).
def playersPiece(gameState: GameState, x: Int, y: Int) : (Boolean,Boolean) = {
  val piece = gameState._1(x)(y).toInt
  (piece == gameState._2 || piece == gameState._2 + 2, piece == 1 || piece == 2)
}

// Checks whether a cell is occupied or empty.
def isOccupied(board: Array[Array[String]], x: Int, y: Int) : Boolean = {
  board(x)(y).toInt != 0
}

// Checks if a move is forward by comparing the row indices of displacement.
def isForward(player: Int, ir: Int, fr: Int) : Boolean = {
  (player == 1 && ir > fr) || (player == 2 && ir < fr)
}

// Checks if a location has an opponent.
def isOpponent(gameState: GameState, x: Int, y: Int) : Boolean = {
  val piece = gameState._1(x)(y).toInt
  if (gameState._2 == 1)
    piece == 2 || piece == 4
  else
    piece == 1 || piece == 3
}

// Mini controller to check and transfer states for consecutive jumps.
// Assumptions: Player always owns the piece, and the initial position is always black.
@tailrec
def jumpsController(currState: GameState, rows: IndexedSeq[Int], cols: IndexedSeq[Int], pawn: Boolean) : (GameState, Boolean) = {
  // Base case : if there is only one cell remaining, this means that all previous moves
  // have passed, so we return the passed board with players switched and true.
  if (rows.tail.isEmpty) return ((currState._1 , 3-currState._2) , true)
  // Prepare values for checks
  val (ri,ci,rf,cf) = (rows(0),cols(0),rows(1),cols(1))
  val (rMid,cMid) = ((ri+rf)/2,(ci+cf)/2)
  val newBoard = currState._1
  // If the next destination is white, occupied, does not have distance (|2|,|2|),
  // or doesn't have an opponent in the middle of the jump, then it is not valid.
  if (isWhite(rf,cf) || isOccupied(newBoard,rf,cf) ||
    Math.abs(ri-rf) != 2 || Math.abs(ci-cf) != 2 ||
    !isOpponent(currState,rMid,cMid))
    return (currState,false)
  // Run pawn checks
  val piece = newBoard(ri)(ci).toInt
  var stillPawn = pawn // Temporary boolean to pass to the next jump.
  if (pawn) {
    // If the move is not forward, then it's invalid.
    if (!isForward(piece,ri,rf)) {
      println("Illegal backward move!")
      return (currState,false)
    }
    // If the destination is the opposing row, transform to a King.
    if((piece == 1 && rf == 0) || (piece == 2 && rf == 7)) {
      println("Piece has turned to a King! This piece can now move backwards.")
      newBoard(ri)(ci) = (piece + 2).toString
      stillPawn = false
    }
  }
  // Move the jumping piece and remove the enemy that was jumped.
  newBoard(rf)(cf) = newBoard(ri)(ci)
  newBoard(rMid)(cMid) = "0"
  newBoard(ri)(ci) = "0"
  // Test the rest of the moves and return the resulting state.
  jumpsController((newBoard,currState._2), rows.tail, cols.tail, stillPawn)
}

/* --------------------------------------------------- CONTROLLER --------------------------------------------------- */

def checkersController(currState: GameState, input: String) : (GameState, Boolean) = {
  val move = input.replaceAll(" ", "")
  val stepPattern: Regex = "^([1-8])([a-hA-H])([1-8])([a-hA-H])$".r
  val jumpsPattern: Regex = "^([1-8][a-hA-H])([1-8][a-hA-H])+$".r
  val currPlayer = currState._2
  val currBoard = currState._1

  move match {
    // ----------------- Step move -----------------
    case stepPattern(initialRow,initialColumn,finalRow,finalColumn) =>
      val (ri,ci,rf,cf) =
        (getRow(initialRow ,8), getCol(initialColumn.charAt(0)),
        getRow(finalRow, 8), getCol(finalColumn.charAt(0)))
      val (ownership,pawn) = playersPiece(currState,ri,ci)
      // We can include the case of a single jump in this pattern
      if (Math.abs(ri-rf) == 2 && Math.abs(ci-cf) == 2) {
        if (isWhite(ri,ci) || !ownership) return (currState,false)
        else return jumpsController(currState, IndexedSeq(ri,rf), IndexedSeq(ci,cf), pawn)
      }
      // Invalid move if any cell is white, player doesn't have a piece
      // in the initial position, or distance covered is not (|1|,|1|).
      if (isWhite(ri,ci) || isWhite(rf,cf) || !ownership ||
        isOccupied(currBoard,rf,cf) || Math.abs(ci-cf) != 1 ||
        Math.abs(ri-rf) != 1)
        return (currState,false)
      // If the moved piece is a pawn, check that the step is forwards only,
      // and if the pawn reached the last row, convert it to a king.
      if (pawn) {
        if (!isForward(currPlayer,ri,rf)) {
          println("Illegal backward move!")
          return (currState,false)
        }
        if ((currPlayer == 1 && rf == 0) || (currPlayer == 2 && rf == 7)) {
          currBoard(ri)(ci) = (currBoard(ri)(ci).toInt + 2).toString
          println("Piece has turned to a King! This piece can now move backwards.")
        }
      }
      currBoard(rf)(cf) = currBoard(ri)(ci)
      currBoard(ri)(ci) = "0"
      ((currBoard , 3 - currPlayer) , true) // Return

    // ----------------- Jump series -----------------
    case jumpsPattern(_*) =>
      val rows: IndexedSeq[Int] = move.filter(_ < 65).map(56 - _.toInt)
      val cols: IndexedSeq[Int] = move.filter(_ > 64).map(getCol)
      val (ri,ci) = (rows(0),cols(0))
      val (ownership,pawn) = playersPiece(currState,ri,ci)
      if (isWhite(ri,ci) || !ownership) return (currState,false)
      // Because recursion alters the mutable board, we deep copy the
      // current state into a temporary state and use it instead.
      val tempBoard = copyState(currBoard)
      val (newState,valid) = jumpsController((tempBoard, currPlayer), rows, cols, pawn)
      if (valid)
        (newState,true) // If the moves were valid, return the new state.
      else
        (currState,false) // If the moves were not valid, return the original state.

    // ----------------- Wrong input -----------------
    case _ =>
      (currState, false)
  }
}

/* ----------------------------------------------------- DRAWER ----------------------------------------------------- */

def drawCheckersPiece(piece: String) = piece match{
  case "1" => " \u25CF "
  case "3" => " \u25B2 "
  case "2" => s" ${Console.BLACK}\u25CF "
  case "4" => s" ${Console.BLACK}\u25B2 "
  case "0" => " \u0020 "
  case x: String => x
}

def checkersDrawer(currState: GameState): Unit = {
  println(Console.RED + "\n\nPlayer " + currState._2 + "'s Turn " +
    (if (currState._2 == 1) "(White)" else "(Black)") + ":\n")
  for(row <- currState._1.indices) {
    print(Console.RED + (8 - row) + " " + Console.RESET)
    for(col <- currState._1.indices) {
      print((if((row + col) % 2 == 0) "\u001b[48;5;172m" else "\u001b[48;5;130m") +
        drawCheckersPiece(currState._1(row)(col)) + "\u001b[0m")
    }
    println()
  }
  print("   ")
  for(col <- currState._1.indices) {
    print(Console.RED + ('A' + col).toChar + "  ")
  }
  println(Console.RESET)
}
