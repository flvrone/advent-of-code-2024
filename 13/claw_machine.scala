//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.claw_machine

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val machines = buildClawMachines(lines)
  println(
    machines
      .map(tryWinning(_))
      .filter(_.isDefined)
      .map(_.get.spentTokens.toLong)
      .sum
  )

case class Coord(x: Int, y: Int)

case class ClawMachineButton(modifier: Coord, tokenPrice: Int)

case class ClawMachine(
  buttonA: ClawMachineButton,
  buttonB: ClawMachineButton,
  prizeAt: Coord
)

case class ButtonPresses(aPresses: Int, bPresses: Int, spentTokens: Int)

object ButtonPresses:
  def apply(machine: ClawMachine, aPresses: Int, bPresses: Int): ButtonPresses =
    val aTokenCost = machine.buttonA.tokenPrice * aPresses
    val bTokenCost = machine.buttonB.tokenPrice * bPresses
    ButtonPresses(aPresses, bPresses, aTokenCost + bTokenCost)

enum Solution:
  case Unexplored, NotEnough, TooMuch, Goal

case class ABCoord(a: Int, b: Int):
  def increasedList(): List[ABCoord] =
    List(copy(a = a + 1), copy(b = b + 1))

  def decreasedList(): List[ABCoord] =
    List(copy(a = a - 1), copy(b = b - 1))

class ABSolutionMatrix(maxA: Int, maxB: Int):
  val matrix =
    Array.fill(maxA + 1, maxB + 1)(Solution.Unexplored)

  def get(a: Int, b: Int): Option[Solution] =
    if (a < 0 || a > maxA) || (b < 0 || b > maxB) then None
    else Some(matrix.apply(a).apply(b))

  def get(coord: ABCoord): Option[Solution] =
    get(coord.a, coord.b)

  def set(a: Int, b: Int, value: Solution): Unit =
    val subArr = matrix.apply(a)
    subArr(b) = value

  def set(coord: ABCoord, value: Solution): Unit =
    set(coord.a, coord.b, value)

  def render(): Unit =
    println(
      matrix.map(subArr =>
        subArr.map(_ match
          case Solution.Unexplored => "."
          case Solution.NotEnough => "\\"
          case Solution.TooMuch => "/"
          case Solution.Goal => "O"
        ).mkString
      ).mkString("\n")
    )

def buildClawMachines(lines: Seq[String]): List[ClawMachine] =
  val buff = ListBuffer[ClawMachine]()

  var btnAMod = Coord(1, 1)
  var btnBMod = Coord(1, 1)
  for line <- lines do line match
    case s"Button A: X$x, Y$y" => btnAMod = Coord(x.toInt, y.toInt)
    case s"Button B: X$x, Y$y" => btnBMod = Coord(x.toInt, y.toInt)
    case s"Prize: X=$x, Y=$y" =>
      val btnA = ClawMachineButton(btnAMod, tokenPrice = 3)
      val btnB = ClawMachineButton(btnBMod, tokenPrice = 1)
      val prizeAt = Coord(x.toInt, y.toInt)
      buff += ClawMachine(btnA, btnB, prizeAt)
    case _ =>

  buff.toList

def tryWinning(
  machine: ClawMachine, maxEachBtnPresses: Int = 100
): Option[ButtonPresses] =
  val aMod = machine.buttonA.modifier
  val bMod = machine.buttonB.modifier

  val summedMod = addCoords(aMod, bMod)

  val goal = machine.prizeAt

  if  summedMod.x * maxEachBtnPresses < goal.x ||
      summedMod.y * maxEachBtnPresses < goal.y
  then None
  else
    val maxAPresses = maxButtonPresses(aMod, goal)
    val maxBPresses = maxButtonPresses(bMod, goal)
    findSolutions(
      machine,
      ABSolutionMatrix(maxAPresses, maxBPresses),
      List(ABCoord(maxAPresses, 0))
    ).minByOption(_.spentTokens)

def maxButtonPresses(btnMod: Coord, prizeAt: Coord): Int =
  if btnMod.x > btnMod.y then (prizeAt.x / btnMod.x)
  else (prizeAt.y / btnMod.y)

@tailrec
def findSolutions(
  machine: ClawMachine, solMatrix: ABSolutionMatrix, queuedSols: List[ABCoord],
  foundSols: List[ButtonPresses] = List()
): List[ButtonPresses] =
  var newFoundSols = foundSols

  val goal = machine.prizeAt

  val newQueue = queuedSols.flatMap(solCoord =>
    val sol = solMatrix.get(solCoord).get

    if sol != Solution.Unexplored then List()
    else
      val xyCoord = buttonPressesToCoord(machine, solCoord.a, solCoord.b)
      val newSol =
        if xyCoord.x > goal.x || xyCoord.y > goal.y then Solution.TooMuch
        else if xyCoord.x == goal.x && xyCoord.y == goal.y then Solution.Goal
        else Solution.NotEnough

      solMatrix.set(solCoord, newSol)

      val candidates = newSol match
        case Solution.Goal =>
          newFoundSols =
            ButtonPresses(machine, solCoord.a, solCoord.b) :: newFoundSols
          solCoord.decreasedList()
        case Solution.NotEnough => solCoord.increasedList()
        case Solution.TooMuch => solCoord.decreasedList()
        case _ => throw MatchError(newSol)

      candidates.filter(solMatrix.get(_).contains(Solution.Unexplored))
  )

  if newQueue.isEmpty then newFoundSols
  else findSolutions(machine, solMatrix, newQueue, newFoundSols)

def buttonPressesToCoord(
  machine: ClawMachine, aPresses: Int, bPresses: Int
): Coord =
  addCoords(
    multiplyCoord(machine.buttonA.modifier, aPresses),
    multiplyCoord(machine.buttonB.modifier, bPresses),
  )

def addCoords(c1: Coord, c2: Coord): Coord =
  Coord(c1.x + c2.x, c1.y + c2.y)

def multiplyCoord(coord: Coord, times: Int): Coord =
  Coord(coord.x * times, coord.y * times)
