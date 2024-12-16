//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.extraequationbuilder

import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val equations = (
      for line <- lines yield
        val pair = line.split(":\\s+", 2)
        (
          pair.apply(0).toLong,
          pair.apply(1).split("\\s+").map(_.toInt).toList
        )
    ).toList

  println(checkAndSumEquationResults(equations))

enum Operator:
  case Add, Multiply, Concatenate

def checkAndSumEquationResults(equations: List[(Long, List[Int])]): Long =
  equations
    .filter((result, numbers) => isEquationPossible(result, numbers))
    .map((result, numbers) => result)
    .reduceLeft(_ + _)

def isEquationPossible(result: Long, numbers: List[Int]): Boolean =
  val operatorPermutations =
    repetitivePermutations(numbers.length - 1, Operator.values.toList)
  operatorPermutations.exists(calculate(numbers, _) == result)

def calculate(numbers: List[Int], operators: List[Operator]): Long =
  recursiveCalculate(numbers.head, numbers.tail, operators)

@tailrec
def recursiveCalculate(
    currentValue: Long, numbers: List[Int], operators: List[Operator]
  ): Long =
  if numbers.isEmpty then currentValue
  else
    val newValue = applyOperator(operators.head, currentValue, numbers.head)
    recursiveCalculate(newValue, numbers.tail, operators.tail)

def applyOperator(operator: Operator, a: Long, b: Long): Long =
  operator match
    case Operator.Add => a + b
    case Operator.Multiply => a * b
    case Operator.Concatenate => (a.toString() + b.toString()).toLong

def repetitivePermutations[T](length: Int, elements: List[T]): List[List[T]] =
  prependEach(elements, iterations = length)

// Returns new lists, created by prepending every element to the container list,
// and then iterating upon those new lists.
def prependEach[T](
    elements: List[T], containerList: List[T] = List(), iterations: Int = 1
  ): List[List[T]] =
  val newLists = for
    elem <- elements
  yield
    elem :: containerList

  if iterations > 1 then
    newLists.flatMap(prependEach(elements, _, iterations - 1))
  else
    newLists
