//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.patrolloops

import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val (map, position) = buildMapWithPosition(lines)
  println(countPossibleLoops(map, position))

case class Coord(row: Int, col: Int)

enum Direction:
  case Up, Right, Down, Left

case class Position(coord: Coord, dir: Direction)

trait MapTile:
  def passable: Boolean
  def visitedWithDirections: List[Direction]
  def wasVisited: Boolean = visitedWithDirections.nonEmpty

case class Obstacle() extends MapTile:
  val passable = false
  val visitedWithDirections = List()

case class PathTile(
    visitedWithDirections: List[Direction] = List()
  ) extends MapTile:
  val passable = true

case class Tile2DMap(rows: Vector[Vector[MapTile]]):
  def tileAt(row: Int, col: Int): Option[MapTile] =
    if row < 0 || row >= rows.length then None
    else
      val rowVals = rows.apply(row)
      if col < 0 || col >= rowVals.length then None
      else Some(rowVals.apply(col))

  def tileAt(coord: Coord): Option[MapTile] =
    tileAt(coord.row, coord.col)

class Visitable2DMap(tileMap: Tile2DMap):
  val visitableRows = tileMap.rows.map(_.toArray)

  def tileAt(row: Int, col: Int): Option[MapTile] =
    tileMap.tileAt(row, col) match
      case Some(_) => Some(visitableRows.apply(row).apply(col))
      case None => None

  def tileAt(coord: Coord): Option[MapTile] =
    tileAt(coord.row, coord.col)

  def visit(row: Int, col: Int, dir: Direction): Unit =
    val rowValues = visitableRows.apply(row)
    val tile = rowValues.apply(col)
    tile match
      case pt: PathTile =>
        rowValues(col) =
          pt.copy(visitedWithDirections = dir :: pt.visitedWithDirections)
      case Obstacle() =>

  def visit(coord: Coord, dir: Direction): Unit =
    visit(coord.row, coord.col, dir)

  def visit(position: Position): Unit =
    visit(position.coord, position.dir)

  def putObstacleAt(row: Int, col: Int): Unit =
    val rowValues = visitableRows.apply(row)
    rowValues(col) = Obstacle()

  def putObstacleAt(coord: Coord): Unit =
    putObstacleAt(coord.row, coord.col)

  def visitedCoords(): Vector[Coord] =
    (
      for
        (rowVals, row) <- visitableRows.zipWithIndex
        (tile, col) <- rowVals.zipWithIndex.filter(_.apply(0).wasVisited)
      yield
        Coord(row, col)
    ).toVector

def buildMapWithPosition(lines: Seq[String]): (Tile2DMap, Position) =
  var position = Position(Coord(0, 0), Direction.Right)

  val rows = (
    for (line, row) <- lines.zipWithIndex
    yield (
      for (char, col) <- line.zipWithIndex
      yield
        char match
          case '#' => Obstacle()
          case _ =>
            charToDirection(char) match
              case None => PathTile()
              case Some(foundDir) =>
                position = Position(Coord(row, col), foundDir)
                PathTile()
    ).toVector
  ).toVector

  (Tile2DMap(rows), position)

def charToDirection(char: Char): Option[Direction] =
  char match
    case '^' => Some(Direction.Up)
    case 'v' => Some(Direction.Down)
    case '<' => Some(Direction.Left)
    case '>' => Some(Direction.Right)
    case _ => None

def countPossibleLoops(map: Tile2DMap, pos: Position): Int =
  val visitableMap = Visitable2DMap(map)
  isTraversingLooped(visitableMap, pos)

  visitableMap.visitedCoords().count(coord =>
    if coord == pos.coord then false
    else
      val cleanMap = Visitable2DMap(map)
      cleanMap.putObstacleAt(coord)
      isTraversingLooped(cleanMap, pos)
  )

@tailrec
def isTraversingLooped(map: Visitable2DMap, pos: Position): Boolean =
  map.visit(pos)
  val nextCoord = modifyCoordInDirection(pos.coord, pos.dir)
  map.tileAt(nextCoord) match
    case Some(Obstacle()) =>
      val newPos = pos.copy(dir = turnRightRelativeTo(pos.dir))
      isTraversingLooped(map, newPos)
    case Some(pt: PathTile) =>
      if pt.visitedWithDirections.contains(pos.dir) then true
      else
        val newPos = pos.copy(coord = nextCoord)
        isTraversingLooped(map, newPos)
    case None | Some(_) => false

def turnRightRelativeTo(dir: Direction): Direction =
  dir match
    case Direction.Up => Direction.Right
    case Direction.Right => Direction.Down
    case Direction.Down => Direction.Left
    case Direction.Left => Direction.Up

def modifyCoordInDirection(row: Int, col: Int, dir: Direction): (Int, Int) =
  dir match
    case Direction.Up => (row - 1, col)
    case Direction.Right => (row, col + 1)
    case Direction.Down => (row + 1, col)
    case Direction.Left => (row, col - 1)

def modifyCoordInDirection(coord: Coord, dir: Direction): Coord =
  val (row, col) = modifyCoordInDirection(coord.row, coord.col, dir)
  Coord(row, col)
