package org.gengine
package games
import scala.annotation.tailrec
import scala.util.matching.Regex

@tailrec
def findEmptySlot(board: Array[Array[String]], column: Int, currRow: Int) : Int = {
  if (board(currRow+1)(column) != "0") // If the next slot down is not empty, return current row
    currRow
  else
    findEmptySlot(board, column, currRow+1) // If it is empty, recursively call on the next row
}

def connect4Controller(currState: GameState, input: String) : (GameState, Boolean) = {
  val move = input.replaceAll(" ", "")
  val singleDigit: Regex = "^([A-G])$".r
  var column: Int = 0
  move match {
    case singleDigit(col) => column = getCol(col)
    case _ => return (currState, false)
  }
  val currBoard = currState._1
  val currPlayer = currState._2
  val row = findEmptySlot(currBoard,column,-1)
  if (row == -1) return (currState, false)
  currBoard(row)(column) = currPlayer.toString
  ((currBoard,3-currPlayer), true)
}



def connect4Drawer(currState: GameState) : Unit = {
  val board = currState._1
  for(row <- board) {
    for (element <- row) print(element + " ")
    println()
  }
  ('A' to 'G').foreach(_ => print(_))
}