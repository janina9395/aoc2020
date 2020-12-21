sealed trait InputRule {
  def number: Int
}

sealed trait RuleMatcher {
  def matches(in: String): Boolean
  def size: Int
}

case class StringRule(number: Int, s: String) extends InputRule with RuleMatcher {
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

  def matchRules(in: String, offset: Int, rules: List[RuleMatcher]): Boolean = {
    rules match {
      case Nil => offset == in.length // we are here only if prev rules have been passed
      case (r: InfiniteRule) :: rest =>
        (offset + r.size to in.length by r.size).exists { len =>
          if (r.matches(in.substring(offset, len)))
              matchRules(in, len, rest)
          else
            false
        }
      case r :: rest =>
        if (r.matches(in.substring(offset, offset + r.size)))
          matchRules(in, offset + r.size, rest)
        else
          false
    }
  }

  private def matches(in: String, rules: Seq[RuleMatcher]): Boolean = {
    matchRules(in, 0, rules.toList)
  }

  override def matches(in: String): Boolean = {
    matches(in, left) || (right.nonEmpty && matches(in, right))
  }

  override def size: Int = left.map(_.size).sum
}

case class InfiniteRule(
    number: Int,
    rules: Seq[RuleMatcher]
) extends RuleMatcher {

  override def matches(in: String): Boolean = {
    val n = in.length / size
    rules.toList match {
      case r1 :: Nil =>
        ComposedRule(0, left = Array.fill[RuleMatcher](n)(r1).toSeq, Seq.empty)
          .matches(in)
      case r1 :: r2 :: Nil =>
        val repeated1 = Array.fill[RuleMatcher](n)(r1).toSeq
        val repeated2 = Array.fill[RuleMatcher](n)(r2).toSeq
        ComposedRule(0, left = repeated1 ++ repeated2, Seq.empty).matches(in)
    }
  }

  override def size: Int = rules.map(_.size).sum
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

    def resolveOne(ruleNumber: Int): Option[RuleMatcher] = {
      allRules.get(ruleNumber).map(resolveRule(_, allRules))
    }

    def resolve(ruleNumbers: Seq[Int]): Seq[RuleMatcher] = {
      ruleNumbers.flatMap(resolveOne)
    }

    rule match {
      case x: StringRule => x
      case IntRule(num, rules, Nil) =>
        ComposedRule(num, resolve(rules), Seq.empty)
      case IntRule(num, rules, altRules) =>
        if (altRules.contains(num)) { // the only case we have
          InfiniteRule(num, resolve(rules))
        } else {
          ComposedRule(num, resolve(rules), resolve(altRules))
        }
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

  def part2(filename: String, ruleFile: String): Int = {
    val allRules = (
      FileReader.read[InputRule](ruleFile) ++
        Seq("8: 42 | 42 8", "11: 42 31 | 42 11 31").map(ruleMapper.map)
    )
      .map(r => (r.number, r))
      .toMap

    val inputs = FileReader.readLines(filename)
    lazy val ruleZero = resolveRule(allRules(0), allRules)

    inputs.count(s => {
      val res = ruleZero.matches(s)
      res
    })
  }

  assert(part1("sample_day19", "sample_day19_rules") == 2)
  println(s"Part1: ${part1("input_day19", "input_day19_rules")}")

  assert(part2("sample_day19_2", "sample_day19_rules_2") == 12)
  println(s"Part2: ${part2("input_day19", "input_day19_rules")}")

}
