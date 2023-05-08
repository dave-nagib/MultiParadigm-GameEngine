package org.gengine
package games
import scala.util.matching.Regex

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

def eightQueensController(currState: GameState, input: String) : (GameState, Boolean) = {
  val move: String = input.replaceAll(" ", "")
  val putPattern: Regex = "^put([1-8])([A-B])$".r
  val removePattern: Regex = "^remove([1-8])([A-B])$".r
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

def eightQueensDrawer(currState: GameState) : Unit = {

}