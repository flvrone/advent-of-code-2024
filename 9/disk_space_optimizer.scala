//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.diskspaceoptimizer

import scala.annotation.tailrec

@main
def main2(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val map = buildMap(lines.apply(0))

  println(mapChecksum(optimizeMap(map)))

def buildMap(denseMap: String): Vector[Int] =
  var currentId = 0
  var isFile = true
  (
    for blockLengthChar <- denseMap
    yield
      val blockLength = blockLengthChar.toString.toInt
      if isFile then
        val blocks = Vector.fill(blockLength)(currentId)
        currentId += 1
        isFile = false
        blocks
      else
        isFile = true
        Vector.fill(blockLength)(-1)
  ).flatten.toVector

def optimizeMap(map: Vector[Int]): Vector[Int] =
  val mutableMap = map.toArray
  val fileRange = findPrevFileRange(mutableMap, map.length - 1)
  fileRange match
    case Some(range) => recursiveOptimizeMap(mutableMap, range).toVector
    case None => map

@tailrec
def recursiveOptimizeMap(map: Array[Int], fileRange: Range): Array[Int] =
  if fileRange.start <= 0 then map
  else
    val freeRange = findFreeSpaceForRange(map, fileRange)
    freeRange match
      case None =>
      case Some(range) => moveFileRangeTo(map, fileRange, range)

    val nextFileRange = findPrevFileRange(map, fileRange.start - 1)
    nextFileRange match
      case Some(range) => recursiveOptimizeMap(map, range)
      case None => map

def moveFileRangeTo(map: Array[Int], fileRange: Range, destination: Range): Unit =
  val fileIndices = fileRange.toList
  val destIndices = destination.toList

  for (fromIndex, toIndex) <- fileIndices.zip(destIndices) do
    map(toIndex) = map(fromIndex)
    map(fromIndex) = -1

@tailrec
def findPrevFileRange(map: Array[Int], currentIndex: Int): Option[Range] =
  if currentIndex < 0 then None
  else
    val fileId = map.apply(currentIndex)
    if fileId >= 0 then
      Some(findFileRangeFromEnd(map, currentIndex - 1, currentIndex, fileId))
    else findPrevFileRange(map, currentIndex - 1)

@tailrec
def findFileRangeFromEnd(
  map: Array[Int], currentIndex: Int, foundRangeEnd: Int, fileId: Int
): Range =
  if currentIndex < 0 then (0 to foundRangeEnd)
  else
    if map.apply(currentIndex) != fileId then
      (currentIndex + 1) to foundRangeEnd
    else findFileRangeFromEnd(map, currentIndex - 1, foundRangeEnd, fileId)

@tailrec
def findFreeSpaceForRange(
  map: Array[Int], range: Range, currentIndex: Int = 0
): Option[Range] =
  if currentIndex >= range.start then None
  else if map.apply(currentIndex) >= 0 then
    findFreeSpaceForRange(map, range, currentIndex + 1)
  else
    val requiredLength = range.length
    val endIndex =
      findFreeSpaceRangeEnd(map, currentIndex + 1, currentIndex, requiredLength)
    val freeSpaceRange = currentIndex to endIndex
    if freeSpaceRange.length >= requiredLength then Some(freeSpaceRange)
    else findFreeSpaceForRange(map, range, endIndex + 1)

@tailrec
def findFreeSpaceRangeEnd(
  map: Array[Int], currentIndex: Int, rangeStartIndex: Int, maxLength: Int
): Int =
  if currentIndex >= map.length then map.length - 1
  else if map.apply(currentIndex) >= 0 then currentIndex - 1
  else if (rangeStartIndex to currentIndex).length >= maxLength then currentIndex
  else findFreeSpaceRangeEnd(map, currentIndex + 1, rangeStartIndex, maxLength)

def mapChecksum(map: Vector[Int]): Long =
  var checksum: Long = 0
  for
    (fileId, index) <- map.zipWithIndex
    if fileId > 0
  do
    checksum += fileId * index

  checksum
