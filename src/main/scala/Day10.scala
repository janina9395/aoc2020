import scala.+:
import scala.collection.mutable
import scala.io.Source

object Day10 extends App {

  private def readFile(filename: String): Seq[Int] = {
    Source
      .fromResource(filename)
      .getLines
      .map(_.toInt)
      .toSeq
  }

  def part1(input: Seq[Int]) = {
    var last = 0
    var counts = Map[Int, Int]().withDefaultValue(0)
    val sorted = input.sorted
    for (i <- sorted.indices if (1 to 3).contains(sorted(i) - last)) {
      val diff = sorted(i) - last
      last = sorted(i)
      counts += diff -> (counts(diff) + 1)
    }
    counts(1) * (counts(3) + 1)
  }

  def part2(input: Seq[Int]) = {
    val dev = input.max + 3
    val sorted = (input :+ dev).sorted
    var counts = mutable.Map[Int, Long](0 -> 1L).withDefaultValue(0)

    sorted.foreach { it =>
      counts += it -> (counts(it - 1) + counts(it - 2) + counts(it - 3))
    }
    counts(dev)
  }

  val sample = readFile("sample_day10")
  assert(part1(sample) == 220)

  val input = readFile("input_day10")
  println(s"Part1: ${part1(input)}")

  assert(part2(sample) == 19208)
  println(s"Part2: ${part2(input)}")
}
