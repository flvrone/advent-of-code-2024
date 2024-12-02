//> using scala 3.6.1
//> using toolkit 0.6.0

package advent24.dampenedcheck

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.TreeSet

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / inputFileName
  val lines: Seq[String] = os.read.lines(path)

  val buff = new ListBuffer[List[Int]]()

  for line <- lines do
    buff += line.split("\\s+").map(_.toInt).toList

  println(dampenedSafeReports(buff.toList))

enum LevelsTrend:
  case Unknown, Increasing, Decreasing, Broken

def dampenedSafeReports(reports: List[List[Int]]): Int =
  reports.count(isSafeReport)

def isSafeReport(report: List[Int]): Boolean =
  dampenedCheckReportTrend(report) match
    case LevelsTrend.Increasing | LevelsTrend.Decreasing => true
    case _ => false

def dampenedCheckReportTrend(report: List[Int]): LevelsTrend =
  val faultyIndices = TreeSet[Int]()
  var trend = LevelsTrend.Unknown

  def markFaulty(idx: Int): Unit =
    trend = LevelsTrend.Unknown
    faultyIndices.add(idx)
    faultyIndices.add(idx + 1)
    if idx > 0 then faultyIndices.add(idx - 1)

  for
    (a, idx) <- report.zipWithIndex
    if idx < report.length - 1
  do
    val b = report.apply(idx + 1)

    if a == b || (a - b).abs > 3 then
      markFaulty(idx)

    trend match
      case LevelsTrend.Increasing =>
        if a > b then markFaulty(idx)
      case LevelsTrend.Decreasing =>
        if a < b then markFaulty(idx)
      case LevelsTrend.Unknown =>
        trend = if a < b then LevelsTrend.Increasing
                else LevelsTrend.Decreasing
      case LevelsTrend.Broken =>

  if faultyIndices.isEmpty then trend
  else
    var resultingTrend = LevelsTrend.Broken
    faultyIndices.find(idx =>
      val result = checkReportTrend(deleteAt(report, idx))

      result match
        case LevelsTrend.Increasing | LevelsTrend.Decreasing =>
          resultingTrend = result
          true
        case _ => false
    )
    resultingTrend

def deleteAt[T](list: List[T], index: Int): List[T] =
  val (first, second) = list.splitAt(index)
  first ++ second.tail

def checkReportTrend(report: List[Int]): LevelsTrend =
  var trend = LevelsTrend.Unknown

  for
    levelsPair <- report.sliding(2)
    if trend != LevelsTrend.Broken
  do
    val a = levelsPair(0)
    val b = levelsPair(1)
    if a == b || (a - b).abs > 3 then
      trend = LevelsTrend.Broken

    trend match
      case LevelsTrend.Increasing =>
        if a > b then trend = LevelsTrend.Broken
      case LevelsTrend.Decreasing =>
        if a < b then trend = LevelsTrend.Broken
      case LevelsTrend.Unknown =>
        trend = if a < b then LevelsTrend.Increasing
                else LevelsTrend.Decreasing
      case LevelsTrend.Broken =>

  trend
