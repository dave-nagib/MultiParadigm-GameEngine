package org.gengine

def getRow(number: String, boardSize: Int) : Int = {
  boardSize - number.toInt
}

def getCol(letter: String) : Int = {
  letter.toInt - 65
}