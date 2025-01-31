//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.robots_renderer

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val map =
    if inputFileName.contains("test") then
      TileMap2D(11, 7)
    else
      TileMap2D(101, 103)

  val robots = buildRobots(lines, map)

  for i <- 0 to 10000 do
    val renderString = renderRobots(map, robots.map(_.makeMoves(i)))
    if renderString.contains("#######") then
      println("---")
      println(s"After $i seconds:")
      println(renderString)
      Thread.sleep(1000)

case class Coord(x: Int, y: Int)

case class TileMap2D(xSize: Int, ySize: Int):
  val middleX = xSize / 2
  val middleY = ySize / 2

case class Robot(position: Coord, velocity: Coord, map: TileMap2D):
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

def renderRobots(map: TileMap2D, robots: List[Robot]): String =
  val mapMatrix: Array[Array[Char]] = Array.fill(map.ySize, map.xSize)('.')

  for robot <- robots do
    val pos = robot.position
    val line = mapMatrix.apply(pos.y)
    line(pos.x) = '#'

  mapMatrix.map(_.mkString).mkString("\n")
