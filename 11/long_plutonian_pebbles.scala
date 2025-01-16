//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.longplutonianpebbles

import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val nums = lines.apply(0).split("\\s+").map(_.toLong).toVector

  val numCounts = compressAndBlink(nums, 75)
  println(numCounts.values.sum)

def compressAndBlink(nums: Vector[Long], iterations: Int = 1): Map[Long, Long] =
  val numCounts =
    nums.groupBy(num => num)
      .map((k, v) => (k, v.length.toLong))
      .to(collection.mutable.Map)
  blink(numCounts, iterations)

@tailrec
def blink(
  numCounts: collection.mutable.Map[Long, Long], iterations: Int = 1
): Map[Long, Long] =
  if iterations <= 0 then numCounts.toMap
  else
    val newCounts = collection.mutable.Map[Long, Long]()

    for (num, count) <- numCounts do
      if num == 0L then addNumCountToMap(newCounts, 1L, count)
      else
        val str = num.toString()
        if isEven(str.length()) then
          val (s1, s2) = middleSplitStr(str)
          addNumCountToMap(newCounts, s1.toLong, count)
          addNumCountToMap(newCounts, s2.toLong, count)
        else
          addNumCountToMap(newCounts, num * 2024L, count)

    blink(newCounts, iterations - 1)

def addNumCountToMap(
  numCounts: collection.mutable.Map[Long, Long], num: Long, count: Long
): Unit =
  numCounts.get(num) match
    case None => numCounts(num) = count
    case Some(prevCount) => numCounts(num) = prevCount + count

def isEven(num: Int): Boolean = num % 2 == 0

def middleSplitStr(str: String): (String, String) =
  str.splitAt(str.length() / 2)
