//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.dosanddonts

import scala.util.matching.Regex

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  // Easy solution, but that combined string is probably too long?
  println(parseAndSum(lines.mkString))

val MulPattern: Regex = raw"mul\((\d{1,3}),(\d{1,3})\)".r
val DoPattern: String = raw"do\(\)"
val DontPattern: String = raw"don't\(\)"

def parseAndSum(command: String): Int =
  sumOfMults(parsePairs(filterCommand(command)))

def parsePairs(command: String): List[(Int, Int)] =
  MulPattern.findAllMatchIn(command).toList.map(ptrnMatch =>
    (ptrnMatch.group(1).toInt, ptrnMatch.group(2).toInt)
  )

def sumOfMults(pairs: List[(Int, Int)]): Int =
  pairs.map((a, b) => a * b).reduceLeft(_ + _)

def filterCommand(command: String): String =
  command.split(DoPattern)
    .map(_.split(DontPattern, 2).apply(0))
    .mkString
