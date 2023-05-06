//package org.gengine
//package games
//import scala.util.matching.Regex
//
//
//
//def checkersController(currBoard: Array[Array[String]], currPlayer: Int, input: String) : (Array[Array[String]], Int, Boolean) = {
//  val move = input.replaceAll(" ", "")
//  val stepPattern: Regex = "^([1-8])([A-H])([1-8])([A-H])$".r
//  val jumpsPattern: Regex = "^([1-8][A-H])([1-8][A-H])+$".r
//  var ri,ci,rf,cf = 0
//  var jumpCells: List[List[Int]] = List(List())
//
//  move match {
//    case stepPattern(initialRow,initialColumn,finalRow,finalColumn) => {
//      ri = 8 - initialRow.toInt
//      ci = initialColumn.toInt - 65
//      rf = 8 - finalRow.toInt
//      cf = finalColumn.toInt - 65
//    }
//
//    case jumpsPattern(cells) => {
//      cells.foreach(cell: String =>{
//
//      })
//    }
//    case _ => return (currBoard, currPlayer, false)
//  }
//
//  // If the move has white square (indices have the same parity) or player is moving an illegal piece
//  if ((rf%2 == 0 && cf%2 == 0) || (rf%2 == 1 && cf%2 == 1)
//    || (ri%2 == 0 && ci%2 == 0) || (ri%2 == 1 && ci%2 == 1)
//    || (currBoard(ri)(ci) != currPlayer))
//    return (currBoard, currPlayer, false)
//
//}
//
//def checkersDrawer(currState: GameState) : Unit = {
//
//}