package org.gengine
package games
import scala.util.matching.Regex

/* ------------------------------------------------ UTILITY FUNCTIONS ------------------------------------------------ */

def directedCheck(board: Array[Array[String]], currRow: Int, currCol: Int, rowOffset: Int, colOffset: Int) : Boolean = {
  try {
    // Return the current cell is empty AND the next checks are all true
    board(currRow)(currCol) == "0" &&
      directedCheck(board, currRow + rowOffset, currCol + colOffset, rowOffset, colOffset)
  } catch {
    case _: ArrayIndexOutOfBoundsException => true
  }
}

def queenClearCheck(board: Array[Array[String]], row: Int, col: Int) : Boolean = {
  // up-left && up && up-right && left && right && down-left && down && down-right
  directedCheck(board, row-1, col-1, -1, -1) && directedCheck(board, row-1, col, -1, 0) &&
    directedCheck(board, row-1, col+1, -1, 1) && directedCheck(board, row, col-1, 0, -1) &&
    directedCheck(board, row, col+1, 0, 1) && directedCheck(board, row+1, col-1, 1, -1) &&
    directedCheck(board, row+1, col, 1, 0) && directedCheck(board, row+1, col+1, 1, 1)
}

/* --------------------------------------------------- CONTROLLER --------------------------------------------------- */

def eightQueensController(currState: GameState, input: String) : (GameState, Boolean) = {
  val move: String = input.replaceAll(" ", "")
  val putPattern: Regex = "^[pP][uU][tT]([1-8])([a-hA-H])$".r
  val removePattern: Regex = "^[rR][eE][mM][oO][vV][eE]([1-8])([a-hA-H])$".r
  val board = currState._1

  move match {
    case putPattern(r,c) =>
      val (row,col) = (getRow(r,8), getCol(c))
      // If there's no queen there, and the new queen's position
      // won't compromise existing queens, accept the move.
      if (board(row)(col) == "0" && queenClearCheck(board, row, col)) {
        board(row)(col) = "1"
        return ((board,1),true)
      }
      // Otherwise, reject the move.
      (currState,false)

    case removePattern(r,c) =>
      val (row,col) = (getRow(r,8), getCol(c))
      // If there is a queen in the target position, accept the move.
      if (board(row)(col) == "1") {
        board(row)(col) = "0"
        return ((board, 0), true)
      }
      // Otherwise, reject the move.
      (currState,false)

    case _ => (currState, false)
  }
}

/* ----------------------------------------------------- DRAWER ----------------------------------------------------- */

def draw8QueensPiece(piece: String) = piece match{
  case "1" => s"\u2002${Console.BLACK}\u265B\u2002"
  case "0" => "\u2002\u2003\u2002"
  case x: String => x
}

def eightQueensDrawer(currState: GameState) : Unit = {
  println("\n\n\n")
  for (row <- currState._1.indices) {
    print(Console.RED + (8 - row) + " " + Console.RESET)
    for (col <- currState._1.indices) {
      print((if ((row + col) % 2 == 0) "\u001b[48;5;172m" else "\u001b[48;5;130m") + 
        draw8QueensPiece(currState._1(row)(col)) + "\u001b[0m")
    }
    println()
  }
  print("   ")
  for (col <- currState._1.indices) {
    print(Console.RED + ('A' + col).toChar + "  \u2009")
  }
  println(Console.RESET)
}