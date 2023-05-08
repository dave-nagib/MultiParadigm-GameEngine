package org.gengine
import games._
import scala.util.matching.Regex

type GameState = (Array[Array[String]], Int)

def GameEngine(startingState: GameState, drawer: GameState => Unit, controller: (GameState, String) => (GameState, Boolean)): Unit = {  drawer(startingState)
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

@main def main(): Unit = {
  GameEngine(chessStartGenerator(), sudokuDrawer, tictactoeController)
}
