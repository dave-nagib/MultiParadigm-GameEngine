package org.gengine
package games
import scala.util.matching.Regex

def sudokuDrawer(gameState: GameState) = {
  println(" \t" + Console.WHITE_B + "                               " + Console.RESET)
  for (row <- gameState._1.indices) {
    print(Console.RED + (row + 1) + "\t" + Console.RESET)
    for (col <- gameState._1(0).indices) {
      if(col % 3 == 0)
        print(s"${Console.WHITE_B} ")
      print(s"${Console.UNDERLINED}${Console.BLACK_B}${Console.BOLD}${Console.MAGENTA} ${gameState._1(row)(col)} ")
    }
    println(Console.WHITE_B + " " + Console.RESET)
    if((row + 1) % 3 == 0)
      println(" \t" + Console.WHITE_B + "                               " + Console.RESET)
  }
  print(" \t  " + Console.RED)
  for (col <- gameState._1(0).indices) {
    print((col + 'A').toChar + "  ")
    if((col + 1) % 3 == 0)
      print(" ")
  }
  println(Console.RESET)
}

def sudokuController(gameState: GameState, move: String) : (GameState, Boolean) = {
  val (pos: Array[Int], value: String) = ((
    try {
      (new Regex("\\b\\w\\d\\b") findFirstIn move).get
    } catch {
      case x: Exception => {
        println("Input a valid Cell!!")
        return (gameState, false)
      }
    }).map( //use regular expressions to find the required input
    c => { //mapping the string to an indexed sequence of ints representing the x and y coordinates of position of input
      (
        if (c.isLetter) {
          c.toLower - 'a'
        } else
          c - 49
        ) match { //pattern matching output of if condition to make sure that the input is within acceptable range
        case x: Int if x > 8 => return (gameState, false)
        case y => y
      }
    }
  ) match { //mapping the output indexed sequence to an array and returning false if no indexed sequence of length 2 is found
    case IndexedSeq(x: Int, y: Int) => Array(y, x)
    case _ => return (gameState, false)
  },
    try{
      (new Regex("\\b\\d\\b") findFirstIn move).get
    }catch {
      case x: Exception =>
        println("Input a valid value!!")
        return (gameState, false)
    }
  )

  val NW_corner_currentSquare: Array[Int] = pos.map(p => (p / 3) * 3)

  for (row <- NW_corner_currentSquare(0) until NW_corner_currentSquare(0) + 3)
    for (col <- NW_corner_currentSquare(1) until NW_corner_currentSquare(1) + 3)
      if (gameState._1(row)(col) == value)
        return (gameState, false)

  gameState._1(pos(0)).foreach(
    square =>
      if (square == value) return (gameState, false)
  )

  gameState._1.foreach(
    row =>
      if (row(pos(1)) == value)
        return (gameState, false)
  )

  val output: (GameState, Boolean) = ((gameState._1.map(
    row =>
      row.map(identity)
  ), gameState._2 + 1), true)

  output._1._1(pos(0))(pos(1)) = value
  output
}

