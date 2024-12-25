//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.plutonianpebbles

import scala.annotation.tailrec

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val nums = lines.apply(0).split("\\s+").map(_.toLong).toVector

  println(blink(nums, 25).length)

@tailrec
def blink(nums: Vector[Long], iterations: Int = 1): Vector[Long] =
  if iterations <= 0 then nums
  else
    val newNums = nums.flatMap(num =>
      if num == 0L then Vector(1L)
      else
        val str = num.toString()
        if isEven(str.length()) then
          middleSplitStr(str).toList.map(_.toLong)
        else Vector(num * 2024L)
    )
    blink(newNums, iterations - 1)

def isEven(num: Int): Boolean = num % 2 == 0

def middleSplitStr(str: String): (String, String) =
  str.splitAt(str.length() / 2)
