import scala.io.Source

object Day11 extends App {

  private def readFile(filename: String): Seq[String] = {
    Source
      .fromResource(filename)
      .getLines
      .toSeq
  }

  def part1(input: Seq[String]) = {
    0
  }

  def part2(input: Seq[String]) = {
    0
  }

  val sample = readFile("sample_day11")
  assert(part1(sample) == 0)

  val input = readFile("input_day11")
  println(s"Part1: ${part1(input)}")

  assert(part2(sample) == 0)
  println(s"Part2: ${part2(input)}")
}
