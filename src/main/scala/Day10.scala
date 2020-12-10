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
    var counts = Map[Int, Int]()
    val sorted = input.sorted
    for (i <- sorted.indices) {
      val diff = sorted(i) - last
      if ((1 to 3).contains(diff)) {
        last = sorted(i)
        counts += diff -> (counts.getOrElse(diff, 0) + 1)
      }
    }
    println(s"(${counts(1)}, ${counts(3)})")
    counts(1) * (counts(3) + 1)
  }

  // doen't work :-(
  def part2(input: Seq[Int]) = {
    val sorted = input.sorted
    val queue = mutable.Queue[(Int, Int)]((0, -1))
    var count = 0

    while (queue.nonEmpty) {
      val (it, ind) = queue.dequeue()
      if (it == sorted.last) {
        count += 1
      }

      for (i <- ind + 1 until sorted.length; if (sorted(i) - it) <= 3) {
        val diff = sorted(i) - it
        if ((1 to 3).contains(diff)) {
          queue.enqueue(sorted(i) -> i)
        }
      }
    }
    count
  }

  val sample = readFile("sample_day10")
  assert(part1(sample) == 220)

  val input = readFile("input_day10")
  println(s"Part1: ${part1(input)}")

  assert(part2(sample) == 19208)
  println(s"Part2: ${part2(input)}")
}
