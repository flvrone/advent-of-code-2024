//> using scala 3.6.2
//> using toolkit 0.6.0

import scala.collection.mutable.ListBuffer

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val buff1 = new ListBuffer[Int]()
  val buff2 = new ListBuffer[Int]()

  for line <- lines do
    val pair = line.split("\\s+", 2)
    buff1 += pair.apply(0).toInt
    buff2 += pair.apply(1).toInt

  println(totalDistance(buff1.toVector, buff2.toVector))

def totalDistance(l1: Vector[Int], l2: Vector[Int]): Int =
  val s1 = l1.sorted
  val s2 = l2.sorted

  var total = 0

  for i <- 0 until s1.length do
    total += (s1.apply(i) - s2.apply(i)).abs

  total
