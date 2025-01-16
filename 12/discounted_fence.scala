//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.discountedfence

import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val map = Char2DMatrix(lines.toVector)

  println(formPlots(map).map(plotFencePrice(map, _)).reduceRight(_ + _))

case class Coord(row: Int, col: Int)

case class Char2DMatrix(lines: Vector[String]):
  def charAt(row: Int, col: Int): Option[Char] =
    if row < 0 || row >= lines.length then None
    else
      val line = lines.apply(row)
      if col < 0 || col >= line.length() then None
      else Some(line.apply(col))

  def charAt(coord: Coord): Option[Char] =
    charAt(coord.row, coord.col)

  def charAtIs(row: Int, col: Int, char: Char): Boolean =
    charAt(row, col) match
    case Some(foundChar) => char == foundChar
    case None => false

  def charAtIs(coord: Coord, char: Char): Boolean =
    charAtIs(coord.row, coord.col, char)

case class MapTile(identifier: Char, visited: Boolean = false)

class Visitable2DMap(map: Char2DMatrix):
  val visitableRows = map.lines.map(line => line.map(MapTile(_)).toArray)

  def tileAt(row: Int, col: Int): Option[MapTile] =
    map.charAt(row, col) match
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

def formPlots(map: Char2DMatrix): Vector[List[Coord]] =
  val visitableMap = Visitable2DMap(map)
  for
    (line, row) <- map.lines.zipWithIndex
    (char, col) <- line.zipWithIndex
    if !visitableMap.tileAt(row, col).get.visited
  yield gatherPlotCoords(visitableMap, char, Coord(row, col))

def gatherPlotCoords(
  map: Visitable2DMap, identifier: Char, startCoord: Coord
): List[Coord] =
  _gatherPlotCoords(map, identifier, List(startCoord))

@tailrec
def _gatherPlotCoords(
  map: Visitable2DMap, identifier: Char, queuedCoords: List[Coord],
  foundCoords: List[Coord] = List()
): List[Coord] =
  var newFoundCoords = foundCoords

  val newQueue = queuedCoords.flatMap(coord =>
    val tile = map.tileAt(coord).get
    if tile.visited then List()
    else
      map.visit(coord)
      if tile.identifier == identifier then
        newFoundCoords = coord :: newFoundCoords

      val nextCoords = Direction.values.map(modifyCoordInDirection(coord, _))
      nextCoords.filter(nextCoord =>
        map.tileAt(nextCoord) match
          case None => false
          case Some(nextTile) =>
            !nextTile.visited && nextTile.identifier == identifier
      )
  )

  if newQueue.isEmpty then newFoundCoords
  else _gatherPlotCoords(map, identifier, newQueue, newFoundCoords)

def plotFencePrice(map: Char2DMatrix, plot: List[Coord]): Int =
  plot.length * plotSidesCount(map, plot)

def plotSidesCount(map: Char2DMatrix, plot: List[Coord]): Int =
  val identifier = map.charAt(plot.head).get

  horizontalSidesCount(map, plot, identifier) +
    verticalSidesCount(map, plot, identifier)

def horizontalSidesCount(
  map: Char2DMatrix, plot: List[Coord], identifier: Char
): Int =
  (
    for dir <- List(Direction.Up, Direction.Down) yield
      plot
        .filter(coordBordersInDirection(map, _, identifier, dir))
        .groupMap(_.row)(_.col)
        .values
        .map(separateProgressionsCount(_))
        .reduce(_ + _)
  ).reduce(_ + _)

def verticalSidesCount(
  map: Char2DMatrix, plot: List[Coord], identifier: Char
): Int =
  (
    for dir <- List(Direction.Left, Direction.Right) yield
      plot
        .filter(coordBordersInDirection(map, _, identifier, dir))
        .groupMap(_.col)(_.row)
        .values
        .map(separateProgressionsCount(_))
        .reduce(_ + _)
  ).reduce(_ + _)

def coordBordersInDirection(
  map: Char2DMatrix, coord: Coord, identifier: Char, dir: Direction
): Boolean =
  val outerCoord = modifyCoordInDirection(coord, dir)
  map.charAt(outerCoord) match
    case Some(outerIdentifier) => outerIdentifier != identifier
    case None => true

def separateProgressionsCount(nums: List[Int]): Int =
  if nums.length <= 1 then 1
  else
    nums
      .sorted
      .sliding(2)
      .count(pair => (pair.apply(1) - pair.apply(0)) > 1) + 1
