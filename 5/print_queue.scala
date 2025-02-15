//> using scala 3.6.2
//> using toolkit 0.6.0

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val (rules, restLines) = lines.span(_.contains('|'))
  val (whiteSpace, queueLines) = restLines.span(_.isEmpty())

  val rulesMap = buildRulesMap(rules)
  val queues = parseQueues(queueLines)

  println(
    queues.filter(isQueueCorrect(_, rulesMap))
      .map(middleElement(_))
      .sum
  )

def buildRulesMap(rules: Seq[String]): Map[Int, Set[Int]] =
  rules.map(_.split('|'))
    .groupMap(_.apply(0).toInt)(_.apply(1).toInt)
    .map((k, v) => (k, v.toSet))

def parseQueues(queues: Seq[String]): List[Vector[Int]] =
  queues.map(line => line.split(',').map(_.toInt).toVector).toList

def isQueueIncorrect[T](queue: Vector[T], rulesMap: Map[T, Set[T]]): Boolean =
  queue.zipWithIndex.exists((elem, idx) =>
    if rulesMap.contains(elem) then
      val elemsGoingAfter = rulesMap.apply(elem)
      queue.take(idx).exists(elemsGoingAfter.apply(_))
    else false
  )

def isQueueCorrect[T](queue: Vector[T], rulesMap: Map[T, Set[T]]): Boolean =
  !isQueueIncorrect(queue, rulesMap)

def middleElement[T](queue: Vector[T]): T =
  queue.apply(queue.length / 2)
