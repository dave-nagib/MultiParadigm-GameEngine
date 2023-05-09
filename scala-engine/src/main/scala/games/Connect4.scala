package org.gengine
package games
import java.util.Currency
import scala.annotation.tailrec
import scala.util.matching.Regex

/* ------------------------------------------------ UTILITY FUNCTIONS ------------------------------------------------ */

@tailrec
def findEmptySlot(board: Array[Array[String]], column: Int, currRow: Int) : Int = {
  // If the next slot down is not empty or the ground, return current row.
  if (currRow == 5 || board(currRow+1)(column) != "0")
    currRow
  else
    findEmptySlot(board, column, currRow+1) // If it is empty, recursively call on the next row.
}

/* --------------------------------------------------- CONTROLLER --------------------------------------------------- */

def connect4Controller(currState: GameState, input: String) : (GameState, Boolean) = {
  val move = input.replaceAll(" ", "")
  val singleDigit: Regex = "^([a-gA-G])$".r
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

/* ----------------------------------------------------- DRAWER ----------------------------------------------------- */

def drawConnect4Piece(piece: String) = piece match{
  case "0" => s"\u2002${Console.WHITE}\u2B24\u2002"
  case "1" => s"\u2002${Console.RED}\u2B24\u2002"
  case "2" => s"\u2002${Console.YELLOW}\u2B24\u2002"
  case x: String => x
}

def connect4Drawer(currState: GameState) : Unit = {
  println(Console.RED + "\n\nPlayer " + currState._2 + "'s Turn " +
    (if (currState._2 == 1) "(Red)" else "(Yellow)") + ":\n")
  for (row <- currState._1.indices) {
    for (col <- currState._1.indices) {
      print("\u001b[48;5;18m" + drawConnect4Piece(currState._1(row)(col)) + "\u001b[0m")
    }
    println()
  }
  print(" ")
  for (col <- currState._1.indices) {
    print(Console.RED + ('A' + col).toChar + "  ")
  }
  println(Console.RESET)
}