import scala.io.Source

case class Rule(min: Int, max: Int, letter: Char)
case class Line(rule: Rule, password: String) {

  def isValid(): Boolean = {
    val count = password.toCharArray.count(_ == rule.letter)
    count <= rule.max && count >= rule.min
  }

  def isValidByNewPolicy(): Boolean = {
    var count = 0
    if (password.charAt(rule.min - 1) == rule.letter) count = count + 1
    if (password.charAt(rule.max - 1) == rule.letter) count = count + 1
    count == 1
  }
}

object Day2 extends App {

  def readFile(filename: String): Seq[Line] =
    Source.fromResource(filename).getLines.map { line =>
      val args = line.split(":")
      val rangeAndChar = args(0).split(" ")
      val range = rangeAndChar(0).split("-")
      Line(Rule(range(0).toInt, range(1).toInt, rangeAndChar(1).charAt(0)), args(1).trim)
    }.toSeq

  val sample = readFile("sample_day2")
  assert(sample.count(_.isValid()) == 2)
  assert(sample.count(_.isValidByNewPolicy()) == 1)

  val input = readFile("input_day2")
  println(s"Part1: ${input.count(_.isValid())}")
  println(s"Part2: ${input.count(_.isValidByNewPolicy())}")

}
