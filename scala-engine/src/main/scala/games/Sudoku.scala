package org.gengine
package games
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.util.matching.Regex

def sudokuDrawer(gameState: GameState) = {
  println(" \t" + s"${Console.BLACK_B}┏━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┓${Console.RESET}")
  for (row <- gameState._1.indices) {
    print(Console.RED + (row + 1) + "\t" + Console.RESET)
    for (col <- gameState._1(0).indices) {
      if(col % 3 == 0)
        print(s"${Console.BLACK_B}┃${Console.RESET}")
      print(s"${Console.BLACK_B}${Console.MAGENTA} ${gameState._1(row)(col)(0) match
        case '0' => ' '
        case x => x
      } ${Console.RESET}")
      if((col+1) % 3 != 0)
        print(s"${Console.BLACK_B}│${Console.RESET}")
    }
    if((row + 1) % 3 != 0)
      print(s"┃\n \t${Console.BLACK_B}┣───┼───┼───╋───┼───┼───╋───┼───┼───${Console.RESET}")
    if ((row + 1) % 3 == 0 && (row + 1) != gameState._1.length)
      print(s"┃\n \t${Console.BLACK_B}┣━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━${Console.RESET}")
    println("┃")
  }
  println(" \t" + s"${Console.BLACK_B}┗━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┛${Console.RESET}")
  print(" \t  " + Console.RED)
  for (col <- gameState._1(0).indices) {
    print((col + 'A').toChar + "   ")
  }
  println(Console.RESET)
}

@tailrec
def checkVerticalExistence(gameState: GameState, value: String, col: Int, row: Int): Boolean = {
  if(row >= gameState._1.length)
    return false
  if(gameState._1(row)(col) == value)
    return true
  checkVerticalExistence(gameState, value, col, row + 1)
}

def sudokuController(gameState: GameState, move: String) : (GameState, Boolean) = {

  val position = """(?i)(\b(?:\d[a-h])|(?:[a-h]\d)\b)(?-i)""".r.unanchored
  val operation = """(?i)(del|\b\d\b)(?-i)""".r.unanchored

  def delete(pos: Array[Int])(gameState: GameState): (GameState, Boolean) = {
    if (gameState._1(pos(0))(pos(1)).length == 1) {
      return (gameState, false)
    }
    val newState = copyState(gameState._1)
    newState(pos(0))(pos(1)) = " "
    ((newState, gameState._2), true)
  }

  def insert(pos: Array[Int], value: String)(gameState: GameState): (GameState, Boolean) = {
    val NW_corner_currentSquare: Array[Int] = pos.map(p => (p / 3) * 3)

    var invalidOP: Boolean = false

    gameState._1.slice(NW_corner_currentSquare(0), NW_corner_currentSquare(0) + 3)
      .map(row => row.slice(NW_corner_currentSquare(1), NW_corner_currentSquare(1) + 3))
      .foreach(row => {
        if (row.contains(value))
          invalidOP = true
      })

    if(gameState._1(pos(0)).contains(value)) invalidOP = true

    if(checkVerticalExistence(gameState, value, pos(1), 0)) invalidOP = true

    if(invalidOP)
      return (gameState, false)

    val newState = copyState(gameState._1)

    newState(pos(0))(pos(1)) = s"${value}u"
    ((newState, gameState._2), true)
  }

  def copyState(gameState: Array[Array[String]]) : Array[Array[String]] = gameState.map(row => row.map(identity))


  val pos =  (move match
    case position(first) => first
    case _ => return (gameState, false)
    ).map( //use regular expressions to find the required input
    c => { //mapping the string to an indexed sequence of ints representing the x and y coordinates of position of input
      (
        if (c.isLetter) {
          c.toLower - 'a'
        } else
          c - 49
        ) match { //pattern matching output of if condition to make sure that the input is within acceptable range
        case x: Int if x >= gameState._1.length => return (gameState, false)
        case y => y
      }
    }
  ) match { //mapping the output indexed sequence to an array and returning false if no indexed sequence of length 2 is found
    case IndexedSeq(x: Int, y: Int) => Array(y, x)
    case _ => return (gameState, false)
  }

  val op = (
    move match
    case operation(first) => first.toLowerCase() match
      case "del" => delete(pos)
      case value: String if 0 to 9 contains value.toInt => insert(pos, value)
    case _ => return (gameState, false)
    )

  op(gameState)
}

