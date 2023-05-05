package org.gengine

import games.Chess._
import games.Sudoku._

object AbstractGameEngine{

  type GameState = (Array[Array[String]], Int)

  val sudokuGameState: GameState = (Array(Array(" ", " ", " ", "7", "9", " ", "5", " ", " "),
    Array("3", "5", "2", " ", " ", "8", " ", "4", " "),
    Array(" ", " ", " ", " ", " ", " ", " ", "8", " "),
    Array(" ", "1", " ", " ", "7", " ", " ", " ", "4"),
    Array("6", " ", " ", "3", " ", "1", " ", " ", "8"),
    Array("9", " ", " ", " ", "8", " ", " ", "1", " "),
    Array(" ", "2", " ", " ", " ", " ", " ", " ", " "),
    Array(" ", "4", " ", "5", " ", " ", "8", "9", "1"),
    Array(" ", "8", " ", " ", "3", "7", " ", " ", " ")), 0)

  val chessGameState: GameState = (Array(Array(blackRook, blackKnight, blackBishop, blackQueen, blackKing, blackBishop, blackKnight, blackRook),
    Array.fill(8)(blackPawn),
    Array(whiteSquare, whitePawn, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare),
    Array(blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare),
    Array(whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare),
    Array(blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare),
    Array.fill(8)(whitePawn),
    Array(whiteRook, whiteKnight, whiteBishop, whiteQueen, whiteKing, whiteBishop, whiteKnight, whiteRook)), 1)

  def GameEngine(startingState: GameState, drawer: GameState => Unit, controller: (GameState, String) => (GameState, Boolean)): Unit = {
    drawer(startingState)
    var newState: GameState = startingState
    var validMove: Boolean = true
    while(true) {
      var move: String = scala.io.StdIn.readLine("Input Move: ")
      val (tempState, temp_isValid) = controller(newState, move)
      newState = tempState
      validMove = temp_isValid
      while(!validMove) {
        move = scala.io.StdIn.readLine("Wrong Input! Try again.\nInput Move: ")
        val (tempState, temp_isValid) = controller(newState, move)
        newState = tempState
        validMove = temp_isValid
      }
      drawer(newState)
    }
  }
  def main(args: Array[String]): Unit = {
    GameEngine(sudokuGameState, sudokuDrawer, sudokuController)
  }
}

