import scala.io.Source

object Day06 extends App {

  def countAnswersByAnyone(groupInput: String): Int = {
    groupInput
      .split("\n")
      .flatMap(_.toCharArray)
      .toSet
      .size
  }

  def countAnswersByEveryone(groupInput: String): Int = {
    val answers = groupInput.split("\n")
    val peopleInGroup = answers.length
    answers
      .flatMap(_.toCharArray)
      .groupBy(identity)
      .map { case (_, group) => group.length } // total count by answer
      .count(_ == peopleInGroup)
  }

  def readFile(filename: String): Seq[String] =
    Source
      .fromResource(filename)
      .getLines
      .map(_.trim)
      .mkString("\n")
      .split("\n\n")

  val sample = readFile("sample_day6")
  assert(sample.map(countAnswersByAnyone).sum == 11)
  assert(sample.map(countAnswersByEveryone).sum == 6)

  val input = readFile("input_day6")
  println(s"Part1: ${input.map(countAnswersByAnyone).sum}")
  println(s"Part2: ${input.map(countAnswersByEveryone).sum}")
}
