//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.restroom_robots

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val map =
    if inputFileName.contains("test") then
      TileMap2D(11, 7)
    else
      TileMap2D(101, 103)

  val robots = buildRobots(lines, map)
  println(safetyFactor(robots.map(_.makeMoves(100))))

case class Coord(x: Int, y: Int)

case class TileMap2D(xSize: Int, ySize: Int):
  val middleX = xSize / 2
  val middleY = ySize / 2

enum Quadrant:
  case InBetween, TopLeft, TopRight, BottomLeft, BottomRight

case class Robot(position: Coord, velocity: Coord, map: TileMap2D):
  val quadrant: Quadrant =
    if position.x == map.middleX || position.y == map.middleY then
      Quadrant.InBetween
    else if position.x < map.middleX then
      if position.y < map.middleY then Quadrant.TopLeft
      else Quadrant.BottomLeft
    else
      if position.y < map.middleY then Quadrant.TopRight
      else Quadrant.BottomRight

  def makeMoves(movesNumber: Int): Robot =
    val modifier = multiplyCoord(velocity, movesNumber)
    copy(position = transformCoord(position, modifier))

  private def multiplyCoord(coord: Coord, times: Int): Coord =
    Coord(coord.x * times, coord.y * times)

  private def transformCoord(coord: Coord, modifier: Coord): Coord =
    val x = loopedValue(coord.x + modifier.x, map.xSize)
    val y = loopedValue(coord.y + modifier.y, map.ySize)
    Coord(x, y)

  @tailrec
  private def loopedValue(value: Int, loopedBelow: Int): Int =
    if value < 0 then
      loopedValue(loopedBelow + value, loopedBelow)
    else if value >= loopedBelow then
      value % loopedBelow
    else
      value

def buildRobots(lines: Seq[String], map: TileMap2D): List[Robot] =
  val buff = ListBuffer[Robot]()

  for line <- lines do line match
    case s"p=$posX,$posY v=$velX,$velY" =>
      val position = Coord(posX.toInt, posY.toInt)
      val velocity = Coord(velX.toInt, velY.toInt)
      buff += Robot(position, velocity, map)
    case _ =>

  buff.toList

def safetyFactor(robots: List[Robot]): Long =
  val robotsPerQuadrant =
    robots
      .groupBy(_.quadrant)
      .map((quadrant, robots) => (quadrant, robots.length.toLong))

  Quadrant
    .values
    .filterNot(_ == Quadrant.InBetween)
    .map(robotsPerQuadrant.getOrElse(_, 0L))
    .reduce(_ * _)
