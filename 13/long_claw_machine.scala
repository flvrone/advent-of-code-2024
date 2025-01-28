//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.long_claw_machine

import scala.collection.mutable.ListBuffer

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
  def apply(
      machine: ClawMachine, aPresses: Long, bPresses: Long
  ): ButtonPresses =
    val aTokenCost = machine.buttonA.tokenPrice * aPresses
    val bTokenCost = machine.buttonB.tokenPrice * bPresses
    ButtonPresses(aPresses, bPresses, aTokenCost + bTokenCost)

val ClawMachinePrizeModifier = 10_000_000_000_000L

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

// An equation system like this:
//
//  | a1_1 * x + a1_2 * y = b1
// <
//  | a2_1 * x + a2_2 * y = b2
//
// Or 2 matrices, like this:
//
// | a1_1  a1_2 | and | b1 |
// | a2_1  a2_2 |     | b2 |
case class EquationSystem2x2(
  a1_1: Double, a1_2: Double,
  a2_1: Double, a2_2: Double,
  b1: Double, b2: Double
):
  val delta = a1_1 * a2_2 - a1_2 * a2_1

  def isUnsolvable = delta == 0
  def isSolvable = !isUnsolvable

  val deltaX: Option[Double] =
    if isUnsolvable then None
    else
      Some(b1 * a2_2 - a1_2 * b2)

  val x = deltaX.map(_ / delta)

  val deltaY: Option[Double] =
    if isUnsolvable then None
    else
      Some(a1_1 * b2 - b1 * a2_1)

  val y = deltaY.map(_ / delta)

object EquationSystem2x2:
  // Button A: X+94, Y+34
  // Button B: X+22, Y+67
  // Prize: X=8400, Y=5400
  //
  //  | 94a + 22b = 8400
  // <
  //  | 34a + 67b = 5400
  def apply(machine: ClawMachine): EquationSystem2x2 =
    val aMod = machine.buttonA.modifier
    val bMod = machine.buttonB.modifier
    val prizeAt = machine.prizeAt
    EquationSystem2x2(
      aMod.x.toDouble, bMod.x.toDouble,
      aMod.y.toDouble, bMod.y.toDouble,
      prizeAt.x.toDouble, prizeAt.y.toDouble
    )

def tryWinning(machine: ClawMachine): Option[ButtonPresses] =
  val eqSys = EquationSystem2x2(machine)
  if eqSys.isUnsolvable then None
  else
    val aPresses = eqSys.x.get.round
    val bPresses = eqSys.y.get.round

    if aPresses < 0 || bPresses < 0 then None
    else
      val coord = buttonPressesToCoord(machine, aPresses, bPresses)
      if coord == machine.prizeAt then
        Some(ButtonPresses(machine, aPresses, bPresses))
      else
        None

def buttonPressesToCoord(
  machine: ClawMachine, aPresses: Long, bPresses: Long
): Coord =
  addCoords(
    multiplyCoord(machine.buttonA.modifier, aPresses),
    multiplyCoord(machine.buttonB.modifier, bPresses),
  )

def addCoords(c1: Coord, c2: Coord): Coord =
  Coord(c1.x + c2.x, c1.y + c2.y)

def multiplyCoord(coord: Coord, times: Long): Coord =
  Coord(coord.x * times, coord.y * times)
