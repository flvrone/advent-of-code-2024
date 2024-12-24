//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.antinodecheck

import scala.annotation.tailrec

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val matrix = Char2DMatrix(lines.toVector)

  println(countAntinodesOnMap(buildMap(matrix), matrix))

case class Coord(row: Int, col: Int)

case class Char2DMatrix(lines: Vector[String]):
  def isCoordValid(row: Int, col: Int): Boolean =
    if row >= 0 && row < lines.length then
      val line = lines.apply(row)
      col >= 0 && col < line.length()
    else
      false

  def isCoordValid(coord: Coord): Boolean =
    isCoordValid(coord.row, coord.col)

val DigitChars = '0' to '9'
val LowercaseLetters = 'a' to 'z'
val UppercaseLetters = 'A' to 'Z'

def isValidChar(char: Char): Boolean =
  DigitChars.contains(char) ||
  LowercaseLetters.contains(char) ||
  UppercaseLetters.contains(char)

def buildMap(matrix: Char2DMatrix): Map[Char, List[Coord]] =
  (
    for
      (line, row) <- matrix.lines.zipWithIndex
      (char, col) <- line.zipWithIndex
      if isValidChar(char)
    yield
      (char, Coord(row, col))
  ).toList.groupMap(_.apply(0))(_.apply(1))

def countAntinodesOnMap(map: Map[Char, List[Coord]], matrix: Char2DMatrix): Int =
  map.values
    .toList
    .flatMap(findAntinodes(_, matrix))
    .distinct
    .length

def findAntinodes(positions: List[Coord], matrix: Char2DMatrix): List[Coord] =
  recursiveFindAntinodes(positions.head, positions.tail, matrix)

@tailrec
def recursiveFindAntinodes(
  currentPosition: Coord, positions: List[Coord], matrix: Char2DMatrix,
  foundAntinodes: List[Coord] = List()
): List[Coord] =
  if positions.isEmpty then foundAntinodes
  else
    val newAntinodes =
      positions.flatMap(calculateAntinodesFor(currentPosition, _, matrix))
    recursiveFindAntinodes(
      positions.head, positions.tail, matrix, newAntinodes ::: foundAntinodes
    )

def calculateAntinodesFor(
  pos1: Coord, pos2: Coord, matrix: Char2DMatrix
): List[Coord] =
  val coord1 = Coord(
    -2 * (pos1.row - pos2.row) + pos1.row,
    -2 * (pos1.col - pos2.col) + pos1.col
  )
  val coord2 = Coord(
    -2 * (pos2.row - pos1.row) + pos2.row,
    -2 * (pos2.col - pos1.col) + pos2.col
  )
  List(coord1, coord2).filter(matrix.isCoordValid(_))
