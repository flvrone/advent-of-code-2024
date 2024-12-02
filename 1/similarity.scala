//> using scala 3.6.1
//> using toolkit 0.6.0

import scala.collection.mutable.ListBuffer

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / inputFileName
  val lines: Seq[String] = os.read.lines(path)

  val buff1 = new ListBuffer[Int]()
  val buff2 = new ListBuffer[Int]()

  for line <- lines do
    val pair = line.split("\\s+", 2)
    buff1 += pair.apply(0).toInt
    buff2 += pair.apply(1).toInt

  println(similarityScore(buff1.toVector, buff2.toVector))

def similarityScore(l1: Vector[Int], l2: Vector[Int]): Int =
  var score = 0

  for num <- l1 do
    score += num * l2.count(_ == num)

  score
