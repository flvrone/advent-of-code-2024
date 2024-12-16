//> using scala 3.6.2
//> using toolkit 0.6.0

import scala.annotation.tailrec

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val (map, position) = buildMapWithPosition(lines)

  traverseMapFromPosition(map, position)

  val visitedCount =
    map.rows.map(_.count(_ == MapTile.VisitedPath)).reduce(_ + _)

  println(visitedCount)

case class Coord(row: Int, col: Int)

enum Direction:
  case Up, Right, Down, Left

case class Position(coord: Coord, dir: Direction)

enum MapTile:
  case Obstacle, Path, VisitedPath

case class Visitable2DMap(rows: Vector[Array[MapTile]]):
  def rowAt(idx: Int): Option[Array[MapTile]] =
    if idx >= 0 && idx < rows.length then
      Some(rows.apply(idx))
    else
      None

  def tileAt(row: Int, col: Int): Option[MapTile] =
    rowAt(row) match
      case Some(subArr) =>
        if col >= 0 && col < subArr.length then
          Some(subArr.apply(col))
        else None
      case None => None

  def tileAt(coord: Coord): Option[MapTile] =
    tileAt(coord.row, coord.col)

  def visit(row: Int, col: Int): Unit =
    val subArr = rows.apply(row)
    subArr(col) = MapTile.VisitedPath

  def visit(coord: Coord): Unit =
    visit(coord.row, coord.col)

def buildMapWithPosition(lines: Seq[String]): (Visitable2DMap, Position) =
  var position = Position(Coord(0, 0), Direction.Right)

  val rows = (
    for (line, row) <- lines.zipWithIndex
    yield (
      for (char, col) <- line.zipWithIndex
      yield
        char match
          case '#' => MapTile.Obstacle
          case _ =>
            charToDirection(char) match
              case None => MapTile.Path
              case Some(foundDir) =>
                position = Position(Coord(row, col), foundDir)
                MapTile.VisitedPath
    ).toArray
  ).toVector

  (Visitable2DMap(rows), position)

def charToDirection(char: Char): Option[Direction] =
  char match
    case '^' => Some(Direction.Up)
    case 'v' => Some(Direction.Down)
    case '<' => Some(Direction.Left)
    case '>' => Some(Direction.Right)
    case _ => None

@tailrec
def traverseMapFromPosition(map: Visitable2DMap, position: Position): Unit =
  val nextCoord = modifyCoordInDirection(position.coord, position.dir)
  map.tileAt(nextCoord) match
    case Some(MapTile.Obstacle) =>
      val newPosition = position.copy(dir = turnRightRelativeTo(position.dir))
      traverseMapFromPosition(map, newPosition)
    case Some(MapTile.Path) | Some(MapTile.VisitedPath) =>
      map.visit(nextCoord)
      val newPosition = position.copy(coord = nextCoord)
      traverseMapFromPosition(map, newPosition)
    case None =>

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
