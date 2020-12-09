import scala.io.Source

object Day9 extends App {

  private def readFile(filename: String): Seq[Long] = {
    Source
      .fromResource(filename)
      .getLines
      .map(_.toLong)
      .toSeq
  }

  def checkNumberInSum(
      input: Seq[Long],
      number: Long,
      index: Int,
      preambleLen: Int
  ): Boolean = {
    for (
      i <- index until index + preambleLen; j <- i + 1 until index + preambleLen
    ) {
      if (input(i) + input(j) == number)
        return true
    }
    false
  }

  def part1(input: Seq[Long], preambleLen: Int): Option[Long] = {
    input.zipWithIndex
      .drop(preambleLen)
      .map { case (n, i) =>
        if (!checkNumberInSum(input, n, i - preambleLen, preambleLen))
          Some(n)
        else
          None
      }
      .find(_.isDefined)
      .flatten
  }

  def part2(input: Seq[Long], sum: Long): Option[Long] = {
    for (seqLen <- 2 until input.length; i <- input.indices) {
      val s = input.slice(i, i + seqLen)
      if (s.sum == sum) {
        return Some(s.min + s.max)
      }
    }
    None
  }

  val sample = readFile("sample_day9")
  assert(part1(sample, 5).get == 127)

  val input = readFile("input_day9")
  val res1: Long = part1(input, 25).get
  println(s"Part1: $res1")

  assert(part2(sample, 127).get == 62)
  println(s"Part2: ${part2(input, res1).get}")
}
