//> using scala 3.6.2
//> using toolkit 0.6.0

import scala.collection.mutable.ListBuffer

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val buff1 = ListBuffer[Int]()
  val buff2 = ListBuffer[Int]()

  for line <- lines do
    val pair = line.split("\\s+", 2)
    buff1 += pair.apply(0).toInt
    buff2 += pair.apply(1).toInt

  println(similarityScore(buff1.toList, buff2.toList))

def similarityScore(l1: List[Int], l2: List[Int]): Int =
  var score = 0

  for num <- l1 do
    score += num * l2.count(_ == num)

  score
