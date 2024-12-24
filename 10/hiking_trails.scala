//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.hikingtrails

import scala.annotation.tailrec

@main
def main1(inputFileName: String): Unit =
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

case class MapTile(height: Int, visited: Boolean = false)

case class Visitable2DMap(heightMap: Height2DMap):
  val visitableRows = heightMap.rows.map(row => row.map(MapTile(_)).toArray)

  def tileAt(row: Int, col: Int): Option[MapTile] =
    heightMap.heightAt(row, col) match
      case Some(_) => Some(visitableRows.apply(row).apply(col))
      case None => None

  def tileAt(coord: Coord): Option[MapTile] =
    tileAt(coord.row, coord.col)

  def visit(row: Int, col: Int): Unit =
    val subArr = visitableRows.apply(row)
    val tile = subArr.apply(col)
    subArr(col) = tile.copy(visited = true)

  def visit(coord: Coord): Unit =
    visit(coord.row, coord.col)

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
    findHeightCoords(Visitable2DMap(map), Coord(row, col)).length

  scores.reduceRight(_ + _)

def findHeightCoords(
  map: Visitable2DMap, startCoord: Coord, searchHeight: Int = 9
): List[Coord] =
  _findHeightCoords(map, List(startCoord), searchHeight = searchHeight)

@tailrec
def _findHeightCoords(
  map: Visitable2DMap, queuedCoords: List[Coord],
  searchHeight: Int = 9, foundCoords: List[Coord] = List()
): List[Coord] =
  var newFoundCoords = foundCoords

  val newQueue = queuedCoords.flatMap(coord =>
    val tile = map.tileAt(coord).get
    if tile.visited then List()
    else
      map.visit(coord)

      if tile.height == searchHeight then
        newFoundCoords = coord :: newFoundCoords

      val nextCoords = Direction.values.map(modifyCoordInDirection(coord, _))

      nextCoords.filter(coord =>
        map.tileAt(coord) match
          case None => false
          case Some(nextTile) =>
            !nextTile.visited && nextTile.height <= searchHeight &&
            (nextTile.height - tile.height) == 1
      )
  )

  if newQueue.isEmpty then newFoundCoords
  else _findHeightCoords(map, newQueue, searchHeight, newFoundCoords)
