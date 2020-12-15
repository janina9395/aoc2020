import scala.io.Source

class Grid(val input: Seq[Array[Char]]) {
  private val height = input.length
  private val width = input.head.length

  private var curX = 0
  private var curY = 0

  def move(stepX: Int, stepY: Int): Unit = {
    curX = curX + stepX
    curY = curY + stepY
  }

  def hasTree: Boolean = input(curY)(curX % width) == '#'

  def canMoveDown: Boolean = curY < height
}

object Day3 extends App {

  def readFile(filename: String): Seq[Array[Char]] =
    Source.fromResource(filename).getLines.map(_.trim.toCharArray).toSeq

  def countTrees(input: Seq[Array[Char]], stepX: Int, stepY: Int): Int = {
    val grid = new Grid(input)
    var count = 0
    while (grid.canMoveDown) {
      if (grid.hasTree) {
        count = count + 1
      }
      grid.move(stepX, stepY)
    }
    count
  }

  def treesOnSlopes(slopes: Seq[(Int, Int)], input: Seq[Array[Char]]): Long = {
    slopes.map { case (x, y) =>
      countTrees(input, x, y).toLong
    }.product
  }

  val sample = readFile("sample_day3")
  assert(countTrees(sample, 3, 1) == 7)

  val input = readFile("input_day3")
  println(s"Part1: ${countTrees(input, 3, 1)}")

  val slopes = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
  assert(treesOnSlopes(slopes, sample) == 336)

  println(s"Part2: ${treesOnSlopes(slopes, input)}")

}
