import scala.io.Source

object Day3 extends App {

  def readFile(filename: String): Seq[Array[Char]] =
    Source.fromResource(filename).getLines.map {line => line.trim.toCharArray}.toSeq

  def countTrees(input: Seq[Array[Char]], startX: Int, startY: Int): Int = {
    val height = input.length;
    val width = input(0).length

    var curX = startX
    var curY = startY

    var count = 0
    while (curY < height) {
      if (input(curY)(curX) == '#') {
        count = count + 1
      }
      curX = curX + startX
      if (curX >= width)
        curX = curX - width
      curY = curY + startY
    }
    count
  }

  def treesOnSlopes(slopes: Seq[(Int, Int)], input: Seq[Array[Char]]): Long = {
    slopes.map {
      case (x, y) => countTrees(input, x, y).toLong
    }.product
  }

  val sample = readFile("sample_day3")
  assert(countTrees(sample, 3, 1) == 7)

  val input = readFile("input_day3")
  println(s"Part1: ${countTrees(input, 3, 1)}")

  val slopes = Seq((1,1), (3,1), (5,1), (7,1), (1,2))
  assert(treesOnSlopes(slopes, sample) == 336)

  println(s"Part2: ${treesOnSlopes(slopes, input)}")

}
