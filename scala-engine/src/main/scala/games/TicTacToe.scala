package org.gengine
package games

def tictactoeDrawer(gameState: GameState): Unit = {
  println(Console.RED + s"Player ${gameState._2 match
    case 1 => 'X'
    case 2 => 'O'
    case x => x
  }'s Turn:${Console.RESET}")
  print(s" \t${Console.BLACK_B}┏")
  for (col <- gameState._1(0).indices){
    if((col + 1) == gameState._1(0).length)
      println(s"━━━━━┓${Console.RESET}")
    else
      print("━━━━━┳")
  }
  for(row <- gameState._1.indices) {
    print(s"${Console.RED}${row + 1}\t${Console.RESET}")
    for (col <- gameState._1(0).indices) {
      if(col == 0)
        print(s"${Console.BLACK_B}┃")
      if ((col + 1) == gameState._1(0).length)
        println(s"""  ${gameState._1(row)(col) match
          case " " => s"${Console.RED}?${Console.RESET}${Console.BLACK_B}"//(row * gameState._1(0).length + col + 1)
          case x => x
        }  ┃${Console.RESET}""")
      else
        print(s"""  ${
          gameState._1(row)(col) match
            case " " => s"${Console.RED}?${Console.RESET}${Console.BLACK_B}"//(row * gameState._1(0).length + col + 1)
            case x => x
        }  ┃""")
    }
    if(row + 1 == gameState._1.length)
      print(s" \t${Console.BLACK_B}┗")
      for (col <- gameState._1(0).indices) {
        if ((col + 1) == gameState._1(0).length)
          println(s"━━━━━┛${Console.RESET}")
        else
          print("━━━━━┻")
      }
    else
      for (col <- gameState._1(0).indices) {
        if(col == 0)
          print(s" \t${Console.BLACK_B}┣")
        if ((col + 1) == gameState._1(0).length)
          println(s"━━━━━┫${Console.RESET}")
        else
          print("━━━━━╋")
      }
  }
  print(" \t   " + Console.RED)
  for (col <- gameState._1(0).indices){
    print((col + 'A').toChar + "     ")
  }
  println(Console.RESET)
}

def tictactoeController(gameState: GameState, move: String): (GameState, Boolean) = {
  val position = """(?i)(\b(?:\d[a-h])|(?:[a-h]\d)\b)(?-i)""".r.unanchored

  val pos = (move match
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

  if(gameState._1(pos(0))(pos(1)) != " ")
    return (gameState, false)

  val newState = copyState(gameState._1)
  newState(pos(0))(pos(1)) = gameState._2 match
    case 1 => "X"
    case 2 => "O"
    case x => x.toString

  ((newState, 3 - gameState._2), true)
}
