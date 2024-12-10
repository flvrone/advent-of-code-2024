//> using scala 3.6.2
//> using toolkit 0.6.0

package advent24.diskfragmenter

@main
def main1(inputFileName: String): Unit =
  val path: os.Path = os.pwd / os.SubPath(inputFileName)
  val lines: Seq[String] = os.read.lines(path)

  val map = buildMap(lines.apply(0))

  println(mapChecksum(fragmentMap(map)))

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

def fragmentMap(map: Vector[Int]): Vector[Int] =
  val mutableMap = map.toArray
  val emptyIndex = findNextEmptyIndex(mutableMap).getOrElse(map.length)
  val fileIndex = findPrevFileIndex(mutableMap, map.length - 1).getOrElse(0)

  recursiveFragmentMap(mutableMap, emptyIndex, fileIndex).toVector

def recursiveFragmentMap(
    map: Array[Int], emptyIndex: Int, fileIndex: Int
  ): Array[Int] =
  if emptyIndex >= fileIndex then map
  else
    map(emptyIndex) = map(fileIndex)
    map(fileIndex) = -1
    val newEmptyIndex =
      findNextEmptyIndex(map, emptyIndex + 1).getOrElse(map.length)
    val newFileIndex = findPrevFileIndex(map, fileIndex - 1).getOrElse(0)
    recursiveFragmentMap(map, newEmptyIndex, newFileIndex)

def findNextEmptyIndex(map: Array[Int], startIndex: Int = 0): Option[Int] =
  if startIndex >= map.length then None
  else if map.apply(startIndex) < 0 then Some(startIndex)
  else findNextEmptyIndex(map, startIndex + 1)

def findPrevFileIndex(map: Array[Int], startIndex: Int): Option[Int] =
  if startIndex < 0 then None
  else if map.apply(startIndex) >= 0 then Some(startIndex)
  else findPrevFileIndex(map, startIndex - 1)

def mapChecksum(map: Vector[Int]): Long =
  var checksum: Long = 0
  for
    (fileId, index) <- map.zipWithIndex
    if fileId > 0
  do
    checksum += fileId * index

  checksum
