package org.gengine
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.Random

def getRow(number: String, boardSize: Int) : Int = {
  boardSize - number.toInt
}

def getCol(letter: Char) : Int = {
  val col = letter.toInt
  if (col < 97) col - 65 else col - 97
}

def copyState(gameState: Array[Array[String]]) : Array[Array[String]] = gameState.map(row => row.map(identity))

// -------------------------------------------- STARTING STATES --------------------------------------------

def chessStartGenerator() : GameState = {
  (Array(Array("BR", "BN", "BB", "BQ", "BK", "BB", "BN", "BR"),
    Array.fill(8)("BP"),
    Array("  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "),
    Array("  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "),
    Array("  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "),
    Array("  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "),
    Array.fill(8)("WP"),
    Array("WR", "WN", "WB", "WQ", "WK", "WB", "WN", "WR")), 1)
}

def checkersStartGenerator() : GameState = {
  (Array(
    Array("0","2","0","2","0","2","0","2"),
    Array("2","0","2","0","2","0","2","0"),
    Array("0","2","0","2","0","2","0","2"),
    Array("0","0","0","0","0","0","0","0"),
    Array("0","0","0","0","0","0","0","0"),
    Array("1","0","1","0","1","0","1","0"),
    Array("0","1","0","1","0","1","0","1"),
    Array("1","0","1","0","1","0","1","0")),
    1)
}

def ticTacToeStartGenerator() : GameState = {
  (Array(
    Array(" ", " ", " "),
    Array(" ", " ", " "),
    Array(" ", " ", " ")
  ), 1)
}

def connect4StartGenerator() : GameState = {
  (Array.fill(6)(Array.fill(7)("0")), 1)
}

def eightQueensStartGenerator() : GameState = {
  (Array.fill(8)(Array.fill(8)("0")), 1)
}

def sudokuStartGenerator(emptySquares: Int = 61): GameState = {
  val board: Array[Array[Int]] = Array.fill(9, 9)(0)
  val r: Array[mutable.Set[Int]] = Array.fill(9)(mutable.Set[Int]())
  val c: Array[mutable.Set[Int]] = Array.fill(9)(mutable.Set[Int]())
  val z: Array[Array[mutable.Set[Int]]] = Array.fill(3, 3)(mutable.Set[Int]())

  for (x <- 0 to 8; y <- 0 to 8)
    if (board(x)(y) != 0)
      setExist(board(x)(y), x, y)

  def setExist(v: Int, x: Int, y: Int): mutable.Set[Int] = {
    r(x) += v
    c(y) += v
    z(x / 3)(y / 3) += v
}

  def fill(x: Int, y: Int): Boolean = {
    if (board(x)(y) == 0) {
      val candidates = Set() ++ (1 to 9) -- r(x) -- c(y) -- z(x / 3)(y / 3)

      @tailrec
      def current(): Boolean = {
        if (candidates.isEmpty)
          false
        else {
          val v = Random.shuffle(candidates.toList).iterator.next
          candidates -= v
          board(x)(y) = v
          setExist(v, x, y)
          val good = if (y < 8) fill(x, y + 1) else if (x < 8) fill(x + 1, 0) else true
          if (good)
            true
          else {
            board(x)(y) = 0
            r(x) -= v
            c(y) -= v
            z(x / 3)(y / 3) -= v
            current()
          }
        }
      }

      current()
    }
    else if (y < 8) fill(x, y + 1) else if (x < 8) fill(x + 1, 0) else true
  }

  fill(0, 0)
  val toRemove = emptySquares match {
    case x: Int if 17 to 81 contains x => x
    case _ => 17
  }

  val rs = Random.shuffle(List.range(0, 81))
  for (i <- 0 until toRemove)
    board(rs(i) / 9)(rs(i) % 9) = 0

  (board.map(
    row => row.map(
      value => value match
        case 0 => " "
        case x: Int => x.toString
    )
  ) , 1)

}

