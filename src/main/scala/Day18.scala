import scala.annotation.tailrec

object Day18 extends App {

  private def readFile(filename: String): Seq[String] = {
    FileReader.readLines(filename)
  }

  def calc(expr: String): Long = {
    val numbers = expr.split("[\\+\\*]").map(_.toLong).iterator
    val operators = expr.toCharArray.filter(s => s == '+' || s == '*').iterator

    var res = numbers.next()
    while (operators.hasNext) {
      res = operators.next() match {
        case '+' => res + numbers.next()
        case '*' => res * numbers.next()
      }
    }
    res
  }

  @tailrec
  def calc2(expr: String): Long = {
    if (!expr.contains('+'))
      calc(expr)
    else {
      val sumReg = """(\d+)\+(\d+)""".r

      var reduced = expr
      sumReg.findFirstIn(expr).foreach {
        case sumReg(n1, n2) =>
          reduced = expr.replaceFirst(sumReg.regex, (n1.toLong + n2.toLong).toString)
      }
      calc2(reduced)
    }
  }

  def eval(expression: String, calcF: String => Long): Long = {
    if (expression.forall(_.isDigit)) {
      expression.toLong
    } else {
      calcF(expression)
    }
  }

  def evalExpr(exp: String, calcF: String => Long): Long = {
    if (exp.contains("(")) {
      var balance = 1
      val beg = exp.indexOf('(')
      var index = beg + 1

      while (balance != 0) {
        val ch = exp(index)
        if (ch == '(') balance += 1
        else if (ch == ')') balance -= 1
        index += 1
      }

      val end = index
      val value = evalExpr(exp.substring(beg + 1, end - 1), calcF)

      val reduced = exp.replace(exp.substring(beg, end), value.toString)
      evalExpr(reduced, calcF)
    } else {
      eval(exp, calcF)
    }
  }

  def part1(filename: String): Long = {
    readFile(filename).map(s =>
      evalExpr(s.replaceAll(" ", ""), calc)
    ).sum
  }

  def part2(filename: String): Long = {
    readFile(filename).map(s =>
      evalExpr(s.replaceAll(" ", ""), calc2)
    ).sum
  }

  assert(evalExpr("2 * 3 + (4 * 5)".replaceAll(" ", ""), calc) == 26)
  assert(evalExpr("5 + (8 * 3 + 9 + 3 * 4 * 3)".replaceAll(" ", ""), calc) == 437)
  assert(evalExpr("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))".replaceAll(" ", ""), calc) == 12240)
  assert(evalExpr("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".replaceAll(" ", ""), calc) == 13632)

  println(s"Part1: ${part1("input_day18")}")

  assert(evalExpr("1 + (2 * 3) + (4 * (5 + 6))".replaceAll(" ", ""), calc2) == 51)
  assert(evalExpr("2 * 3 + (4 * 5)".replaceAll(" ", ""), calc2) == 46)
  assert(evalExpr("5 + (8 * 3 + 9 + 3 * 4 * 3)".replaceAll(" ", ""), calc2) == 1445)
  assert(evalExpr("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))".replaceAll(" ", ""), calc2) == 669060)
  assert(evalExpr("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".replaceAll(" ", ""), calc2) == 23340)
  println(s"Part2: ${part2("input_day18")}")

}
