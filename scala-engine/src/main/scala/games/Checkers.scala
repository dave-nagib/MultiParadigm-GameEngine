package org.gengine
package games
import scala.util.matching.Regex

def isWhite(x: Int, y: Int) : Boolean = {
  // White squares' indices have the same parity
  (x % 2 == 0 && y % 2 == 0) || (x % 2 == 1 && y % 2 == 1)
}

def checkersController(currState: GameState, input: String) : (GameState, Boolean) = {
  val move = input.replaceAll(" ", "")
  val stepPattern: Regex = "^([1-8])([A-H])([1-8])([A-H])$".r
  val jumpsPattern: Regex = "^([1-8][A-H])([1-8][A-H])+$".r
  var ri,ci,rf,cf = 0

  move match {
    case stepPattern(initialRow,initialColumn,finalRow,finalColumn) => {
      ri = getRow(initialRow ,8)
      ci = getCol(initialColumn)
      rf = getRow(finalRow, 8)
      cf = getCol(finalColumn)
    }

    case jumpsPattern(_*) => {
      var rows: IndexedSeq[Int] = move.filter(_ < 65).map(_.toInt-48)
      var cols: IndexedSeq[Int] = move.filter(_ > 64).map(_.toInt-65)
    }

    case _ => return (currState, false)
  }

  // If the move has white square (indices have the same parity) or player is moving an illegal piece
  if (isWhite(ri,ci) || isWhite(rf,cf) || (currState._1(ri)(ci) != currState._2.toString))
    return (currState, false)

  (currState, false)
}

def checkersDrawer(currState: GameState) : Unit = {

}