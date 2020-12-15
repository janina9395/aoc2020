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
    Source
      .fromResource(filename)
      .getLines
      .map { line =>
        line.split(":") match {
          case Array(rule: String, password: String) =>
            rule.split(" ") match {
              case Array(range: String, letter: String) =>
                range.split("-") match {
                  case Array(min: String, max: String) =>
                    Line(
                      Rule(min.toInt, max.toInt, letter.charAt(0)),
                      password.trim
                    )
                }
            }
        }
      }
      .toSeq

  val sample = readFile("sample_day2")
  assert(sample.count(_.isValid()) == 2)
  assert(sample.count(_.isValidByNewPolicy()) == 1)

  val input = readFile("input_day2")
  println(s"Part1: ${input.count(_.isValid())}")
  println(s"Part2: ${input.count(_.isValidByNewPolicy())}")

}
