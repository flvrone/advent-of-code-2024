//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.xcrossmas

import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val matrix = CharMatrix2D(lines.toVector)

  println(crossMatchesInCharMatrix("MAS", matrix))

case class CharMatrix2D(lines: Vector[String]):
  def charAt(row: Int, col: Int): Option[Char] =
    if row >= 0 && row < lines.length then
      val line = lines.apply(row)
      if col >=0 && col < line.length() then
        Some(line.apply(col))
      else None
    else None

  def charAtIs(row: Int, col: Int, char: Char): Boolean =
    charAt(row, col) match
      case Some(foundChar) => char == foundChar
      case None => false

enum Direction:
  case Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft

def crossMatchesInCharMatrix(word: String, matrix: CharMatrix2D): Int =
  val startChar = word.head
  val gap = word.length() - 1
  var matchCount = 0

  for
    (line, row) <- matrix.lines.zipWithIndex
    (char, col) <- line.zipWithIndex
    if char == startChar
  do
    if matchMatrixInDirectionFromPos(word, matrix, Direction.DownRight, row, col) &&
        matchMatrixInDirectionFromPos(word, matrix, Direction.DownLeft, row, col + gap)
    then matchCount += 1

    if matchMatrixInDirectionFromPos(word, matrix, Direction.UpRight, row, col) &&
        matchMatrixInDirectionFromPos(word, matrix, Direction.UpLeft, row, col + gap)
    then matchCount += 1

    if matchMatrixInDirectionFromPos(word, matrix, Direction.DownRight, row, col) &&
        matchMatrixInDirectionFromPos(word, matrix, Direction.UpRight, row + gap, col)
    then matchCount += 1

    if matchMatrixInDirectionFromPos(word, matrix, Direction.DownLeft, row, col) &&
        matchMatrixInDirectionFromPos(word, matrix, Direction.UpLeft, row + gap, col)
    then matchCount += 1

  matchCount

def modifyCoordsInDirection(row: Int, col: Int, dir: Direction): (Int, Int) =
  dir match
    case Direction.Up => (row - 1, col)
    case Direction.UpRight => (row - 1, col + 1)
    case Direction.Right => (row, col + 1)
    case Direction.DownRight => (row + 1, col + 1)
    case Direction.Down => (row + 1, col)
    case Direction.DownLeft => (row + 1, col - 1)
    case Direction.Left => (row, col - 1)
    case Direction.UpLeft => (row - 1, col - 1)

@tailrec
def matchMatrixInDirectionFromPos(
  word: String, matrix: CharMatrix2D, dir: Direction, row: Int, col: Int
): Boolean =
  if word.isEmpty() then true
  else if matrix.charAtIs(row, col, word.head) then
    val (nextRow, nextCol) = modifyCoordsInDirection(row, col, dir)
    matchMatrixInDirectionFromPos(word.tail, matrix, dir, nextRow, nextCol)
  else
    false
