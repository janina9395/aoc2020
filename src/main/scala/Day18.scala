import scala.annotation.tailrec

object Day18 extends App {

  private def readFile(filename: String): Seq[String] = {
    FileReader.readLines(filename)
  }

  def calc(expr: String): Long = {
    val numbers = expr.split("[+*]").map(_.toLong).iterator
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

  // Calculate with + precedence
  @tailrec
  def calc2(expr: String): Long = {
    if (!expr.contains('+'))
      calc(expr)
    else {
      val sumReg = """(\d+)\+(\d+)""".r

      var reduced = expr
      sumReg.findFirstIn(expr).foreach { case sumReg(n1, n2) =>
        reduced =
          expr.replaceFirst(sumReg.regex, (n1.toLong + n2.toLong).toString)
      }
      calc2(reduced)
    }
  }

  def eval(expr: String, calcF: String => Long): Long = {
    if (expr.forall(_.isDigit)) {
      expr.toLong
    } else {
      calcF(expr)
    }
  }

  def evalExpr(input: String, calcF: String => Long): Long = {
    val expr = input.replaceAll(" ", "")

    def findClosingBraceOfExpr(expr: String): Int = {
      var balance = 1
      val beg = expr.indexOf('(')
      var index = beg + 1

      while (balance != 0) {
        val ch = expr(index)
        if (ch == '(') balance += 1
        else if (ch == ')') balance -= 1
        index += 1
      }
      index - 1
    }

    if (expr.contains("(")) {
      val beg = expr.indexOf('(')
      val end = findClosingBraceOfExpr(expr)

      val value = evalExpr(expr.substring(beg + 1, end), calcF)
      val reduced = expr.replace(expr.substring(beg, end + 1), value.toString)

      evalExpr(reduced, calcF)
    } else {
      eval(expr, calcF)
    }
  }

  def part1(filename: String): Long = {
    readFile(filename).map(evalExpr(_, calc)).sum
  }

  def part2(filename: String): Long = {
    readFile(filename).map(evalExpr(_, calc2)).sum
  }

  assert(evalExpr("2 * 3 + (4 * 5)", calc) == 26)
  assert(evalExpr("5 + (8 * 3 + 9 + 3 * 4 * 3)", calc) == 437)
  assert(evalExpr("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", calc) == 12240)
  assert(
    evalExpr("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", calc) == 13632
  )

  println(s"Part1: ${part1("input_day18")}")

  assert(evalExpr("1 + (2 * 3) + (4 * (5 + 6))", calc2) == 51)
  assert(evalExpr("2 * 3 + (4 * 5)", calc2) == 46)
  assert(evalExpr("5 + (8 * 3 + 9 + 3 * 4 * 3)", calc2) == 1445)
  assert(evalExpr("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", calc2) == 669060)
  assert(
    evalExpr("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", calc2) == 23340
  )

  println(s"Part2: ${part2("input_day18")}")

}
