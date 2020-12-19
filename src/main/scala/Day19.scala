import scala.collection.mutable

sealed trait InputRule {
  def number: Int
}

sealed trait RuleMatcher {
  def matches(in: String): Boolean
  def size: Int
}

case class StringRule(number: Int, s: String)
    extends InputRule
    with RuleMatcher {
  override def matches(in: String): Boolean = in == s
  override def size: Int = 1
}

case class IntRule(number: Int, rules: Seq[Int], altRules: Seq[Int] = Seq.empty)
    extends InputRule

case class ComposedRule(
    number: Int,
    left: Seq[RuleMatcher],
    right: Seq[RuleMatcher]
) extends RuleMatcher {

  private def matches(in: String, rules: Seq[RuleMatcher]): Boolean = {
    var offset = 0
    var parts = mutable.Seq[String]()
    rules.foreach { r =>
      parts += in.substring(offset, offset + r.size)
      offset += r.size
    }
    val iterator = parts.iterator
    offset == in.length && rules.forall(
      iterator.hasNext && _.matches(iterator.next())
    )
  }

  override def matches(in: String): Boolean = {
    matches(in, left) || matches(in, right)
  }

  override def size: Int = left.map(_.size).sum
}

object Day19 extends App {

  implicit val ruleMapper = new Mapper[InputRule] {
    val ruleRegex = """(\d+): ([\d+ ]+) ?\|? ?([\d+ ]+)?""".r
    val letterRegex = """(\d+): "([ab])"""".r
    val parseRuleNumbers: String => Seq[Int] =
      s => s.trim.split(" ").filterNot(_.isEmpty).map(_.toInt).toSeq

    override def map(line: String): InputRule = {
      line.trim match {
        case letterRegex(num, ch) =>
          StringRule(num.toInt, ch)

        case ruleRegex(num, left, null) =>
          IntRule(
            num.toInt,
            rules = parseRuleNumbers(left)
          )

        case ruleRegex(num, left, right) =>
          IntRule(
            num.toInt,
            rules = parseRuleNumbers(left),
            altRules = parseRuleNumbers(right)
          )
      }
    }
  }

  def resolveRule(
      rule: InputRule,
      allRules: Map[Int, InputRule]
  ): RuleMatcher = {

    def resolve(ruleNumbers: Seq[Int]): Seq[RuleMatcher] = {
      ruleNumbers
        .flatMap(allRules.get)
        .map(resolveRule(_, allRules))
    }

    rule match {
      case x: StringRule => x
      case IntRule(num, rules, Nil) =>
        ComposedRule(num, resolve(rules), Seq.empty)
      case IntRule(num, rules, altRules) =>
        ComposedRule(num, resolve(rules), resolve(altRules))
    }
  }

  def part1(filename: String, ruleFile: String): Int = {
    val allRules = FileReader
      .read[InputRule](ruleFile)
      .map(r => (r.number, r))
      .toMap

    val inputs = FileReader.readLines(filename)
    val ruleZero = resolveRule(allRules(0), allRules)

    inputs.count(ruleZero.matches)
  }

  // Causes infinite loop
  def part2(filename: String, ruleFile: String): Int = {
    val allRules = (
      FileReader.read[InputRule](ruleFile) ++
        Seq("8: 42 | 42 8", "11: 42 31 | 42 11 31").map(ruleMapper.map)
    )
      .map(r => (r.number, r))
      .toMap

    val inputs = FileReader.readLines(filename)
    val ruleZero = resolveRule(allRules(0), allRules)

    inputs.count(s => ruleZero.matches(s))
  }

  assert(part1("sample_day18", "sample_day18_rules") == 2)

  println(s"Part1: ${part1("input_day18", "input_day18_rules")}")
  //println(s"Part2: ${part2("input_day18", "input_day18_rules")}")

}
