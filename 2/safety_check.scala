//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.safetycheck

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val reports = for line <- lines yield
    line.split("\\s+").map(_.toInt).toList

  println(countSafeReports(reports.toList))

enum LevelsTrend:
  case Unknown, Increasing, Decreasing, Broken

def countSafeReports(reports: List[List[Int]]): Int =
  reports.count(isSafeReport)

def isSafeReport(report: List[Int]): Boolean =
  checkReportTrend(report) match
    case LevelsTrend.Increasing | LevelsTrend.Decreasing => true
    case _ => false

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
