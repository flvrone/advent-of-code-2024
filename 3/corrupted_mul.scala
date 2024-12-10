//> using scala 3.6.2
//> using toolkit 0.6.0

import scala.util.matching.Regex

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  println(lines.map(parseAndSum).reduceLeft(_ + _))

val MulPattern: Regex = raw"mul\((\d{1,3}),(\d{1,3})\)".r

def parseAndSum(command: String): Int =
  sumOfMults(parsePairs(command))

def parsePairs(command: String): List[(Int, Int)] =
  MulPattern.findAllMatchIn(command).toList.map(
    (patternMatch) =>
      (patternMatch.group(1).toInt, patternMatch.group(2).toInt)
    )

def sumOfMults(pairs: List[(Int, Int)]): Int =
  pairs.map((a, b) => a * b).reduceLeft(_ + _)
