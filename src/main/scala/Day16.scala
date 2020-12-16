import scala.collection.mutable
import scala.io.Source

case class TicketRule(name: String, range1: Range, range2: Range) {

  def matching(n: Int): Boolean = {
    range1.contains(n) || range2.contains(n)
  }

  def matchingAll(numbers: Seq[Int]): Boolean = {
    numbers.forall(matching)
  }
}

object Day16 extends App {

  type Ticket = Seq[Int]

  private def readRules(filename: String): Seq[TicketRule] = {
    val ruleRegex = """([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)""".r
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .map {
        case ruleRegex(name, s1, e1, s2, e2) =>
          TicketRule(name, s1.toInt to e1.toInt, s2.toInt to e2.toInt)
      }
  }

  private def readTickets(filename: String): Seq[Ticket] = {
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .map {
        _.split(",").map(_.toInt).toSeq
      }
  }


  def part1(rules: Seq[TicketRule], tickets: Seq[Ticket]): Int = {

    def matchSomeRule(n: Int): Boolean = {
      rules.exists(_.matching(n))
    }

    tickets.flatten.filterNot(matchSomeRule).sum
  }

  def part2(rules: Seq[TicketRule], tickets: Seq[Ticket]): Long = {

    def matchSomeRule(n: Int): Boolean = {
      rules.exists(_.matching(n))
    }

    def isValid(ticket: Ticket): Boolean = {
      ticket.forall(f => matchSomeRule(f))
    }

    val ticket = readTickets("input_day16_ticket").head
    val validTickets = (tickets :+ ticket).filter(isValid)

    val posRulesPerIndex = ticket.indices.map(i => {
      val fields = validTickets.map(_(i))
      val posRules = rules.filter(_.matchingAll(fields))
      i -> posRules
    }).sortBy {
      case (_, rules) => rules.length
    }

    val rulesFound = mutable.Map[TicketRule, Int]()
    posRulesPerIndex.foreach { case (i, rules) =>
      val r = rules.diff(rulesFound.keys.toSeq)
      if (r.length == 1) {
        rulesFound += r.head -> i
      } else {
        println("Error")
      }
    }

    rulesFound.filter {
      case (rule, _) => rule.name.startsWith("departure")
    }.map {
      case (_, index) => ticket(index).toLong
    }.product
  }

  val tickets = readTickets("input_day16_other_tickets")
  val rules = readRules("input_day16_rules")

  println(s"Part1: ${part1(rules, tickets)}")
  println(s"Part2: ${part2(rules, tickets)}")

}
