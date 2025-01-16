//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.distincthikingtrails

import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val rows =
    for line <- lines yield
      (for char <- line yield char.toString().toInt).toVector

  val map = Height2DMap(rows.toVector)

  println(trailheadsTotalScore(map))

case class Coord(row: Int, col: Int)

case class Height2DMap(rows: Vector[Vector[Int]]):
  def heightAt(row: Int, col: Int): Option[Int] =
    if row < 0 || row >= rows.length then None
    else
      val rowVals = rows.apply(row)
      if col < 0 || col >= rowVals.length then None
      else Some(rowVals.apply(col))

  def heightAt(coord: Coord): Option[Int] =
    heightAt(coord.row, coord.col)

enum Direction:
  case Up, Right, Down, Left

def modifyCoordInDirection(row: Int, col: Int, dir: Direction): (Int, Int) =
  dir match
    case Direction.Up => (row - 1, col)
    case Direction.Right => (row, col + 1)
    case Direction.Down => (row + 1, col)
    case Direction.Left => (row, col - 1)

def modifyCoordInDirection(coord: Coord, dir: Direction): Coord =
  val (row, col) = modifyCoordInDirection(coord.row, coord.col, dir)
  Coord(row, col)

def trailheadsTotalScore(map: Height2DMap): Int =
  val scores = for
    (rowVals, row) <- map.rows.zipWithIndex
    (height, col) <- rowVals.zipWithIndex
    if height == 0
  yield
    findHeightCoords(map, Coord(row, col)).length

  scores.sum

def findHeightCoords(
  map: Height2DMap, startCoord: Coord, searchHeight: Int = 9
): List[Coord] =
  _findHeightCoordsWithDups(map, List(startCoord), searchHeight = searchHeight)

@tailrec
def _findHeightCoordsWithDups(
  map: Height2DMap, queuedCoords: List[Coord],
  searchHeight: Int = 9, foundCoords: List[Coord] = List()
): List[Coord] =
  var newFoundCoords = foundCoords

  val newQueue = queuedCoords.flatMap(coord =>
    val height = map.heightAt(coord).get

    if height == searchHeight then
      newFoundCoords = coord :: newFoundCoords

    val nextCoords = Direction.values.map(modifyCoordInDirection(coord, _))

    nextCoords.filter(nextCoord =>
      map.heightAt(nextCoord) match
        case None => false
        case Some(nextHeight) =>
          (nextHeight - height) == 1 &&
            nextHeight <= searchHeight
      )
  )

  if newQueue.isEmpty then newFoundCoords
  else _findHeightCoordsWithDups(map, newQueue, searchHeight, newFoundCoords)
