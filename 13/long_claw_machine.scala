//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.long_claw_machine

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val machines = buildClawMachines(lines)
  println(
    machines
      .map(tryWinning(_))
      .filter(_.isDefined)
      .map(_.get.spentTokens)
      .sum
  )

case class Coord(x: Long, y: Long)

case class ClawMachineButton(modifier: Coord, tokenPrice: Int)

case class ClawMachine(
  buttonA: ClawMachineButton,
  buttonB: ClawMachineButton,
  prizeAt: Coord
)

case class ButtonPresses(aPresses: Long, bPresses: Long, spentTokens: Long)

object ButtonPresses:
  def apply(machine: ClawMachine, aPresses: Long, bPresses: Long): ButtonPresses =
    val aTokenCost = machine.buttonA.tokenPrice * aPresses
    val bTokenCost = machine.buttonB.tokenPrice * bPresses
    ButtonPresses(aPresses, bPresses, aTokenCost + bTokenCost)

val ClawMachinePrizeModifier = 0
// val ClawMachinePrizeModifier = 10_000_000_000_000L

def buildClawMachines(lines: Seq[String]): List[ClawMachine] =
  val buff = ListBuffer[ClawMachine]()

  var btnAMod = Coord(1L, 1L)
  var btnBMod = Coord(1L, 1L)
  for line <- lines do line match
    case s"Button A: X$x, Y$y" => btnAMod = Coord(x.toLong, y.toLong)
    case s"Button B: X$x, Y$y" => btnBMod = Coord(x.toLong, y.toLong)
    case s"Prize: X=$x, Y=$y" =>
      val btnA = ClawMachineButton(btnAMod, tokenPrice = 3)
      val btnB = ClawMachineButton(btnBMod, tokenPrice = 1)
      val prizeAt = Coord(
        x.toLong + ClawMachinePrizeModifier,
        y.toLong + ClawMachinePrizeModifier
      )
      buff += ClawMachine(btnA, btnB, prizeAt)
    case _ =>

  buff.toList

def tryWinning(machine: ClawMachine): Option[ButtonPresses] =
  val aMod = machine.buttonA.modifier
  val maxAPresses = maxButtonPresses(aMod, machine.prizeAt)
  findSolution(machine, maxAPresses)

// Only works with positive, non-zero coordinates.
def maxButtonPresses(btnMod: Coord, prizeAt: Coord): Long =
  if btnMod.x > btnMod.y then (prizeAt.x / btnMod.x)
  else (prizeAt.y / btnMod.y)

@tailrec
def findSolution(
  machine: ClawMachine, aPresses: Long,
  currentSolution: Option[ButtonPresses] = None
): Option[ButtonPresses] =
  if aPresses < 0 then currentSolution
  else
    val goal = machine.prizeAt
    val onlyACoord = multiplyCoord(machine.buttonA.modifier, aPresses)
    val remainingDistance = Coord(goal.x - onlyACoord.x, goal.y - onlyACoord.y)
    val bPresses =
      maxButtonPresses(machine.buttonB.modifier, remainingDistance)
    val onlyBCoord = multiplyCoord(machine.buttonB.modifier, bPresses)

    val newSolution = ButtonPresses(machine, aPresses, bPresses)

    if  currentSolution.isDefined &&
        currentSolution.get.spentTokens < newSolution.spentTokens
    then
      currentSolution
    else if onlyBCoord == remainingDistance then
      findSolution(machine, aPresses - 1, Some(newSolution))
    else
      findSolution(machine, aPresses - 1, currentSolution)

def multiplyCoord(coord: Coord, times: Long): Coord =
  Coord(coord.x * times, coord.y * times)
