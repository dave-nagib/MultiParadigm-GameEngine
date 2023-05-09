package org.gengine
import games.*

import scala.annotation.tailrec
import scala.util.matching.Regex

type GameState = (Array[Array[String]], Int)

@tailrec
def GameEngine(currentState: GameState, drawer: GameState => Unit, controller: (GameState, String) => (GameState, Boolean)): Unit = {
  drawer(currentState)
  val (newState, is_validMove) = controller(currentState, scala.io.StdIn.readLine("Input Move: "))
  if(!is_validMove)
    println(s"${Console.RED}${Console.BOLD}Wrong Input! Try again.${Console.RESET}")
  GameEngine(newState, drawer, controller)
}

def screen_main(): Unit = {
  print(Console.GREEN +
    "Welcome To The Best Scala Game Engine (BSGE™ for short)" +
    "\n\t1. Chess" +
    "\n\t2. Checkers" +
    "\n\t3. Sudoku" +
    "\n\t4. Connect 4" +
    "\n\t5. Tic Tac Toe" +
    "\n\t6. 8 Queens" +
    "\nPlease select the game you want: " + Console.RESET
  )
  scala.io.StdIn.readInt() match
    case 1 => GameEngine(chessStartGenerator(), chessDrawer, chessController)
    case 2 => GameEngine(checkersStartGenerator(), checkersDrawer, checkersController)
    case 3 => GameEngine(sudokuStartGenerator(), sudokuDrawer, sudokuController)
    case 4 => GameEngine(connect4StartGenerator(), connect4Drawer, connect4Controller)
    case 5 => GameEngine(ticTacToeStartGenerator(), tictactoeDrawer, tictactoeController)
    case 6 => GameEngine(eightQueensStartGenerator(), eightQueensDrawer, eightQueensController)
    case _ => GameEngine(chessStartGenerator(), chessDrawer, chessController)
  ()
}

@main def main(): Unit = {
  screen_main()
}
