import scala.collection.mutable
import scala.io.Source

case class Bag(color: String, innerBags: Map[String, Int])

object Day7 extends App {

  def readFile(filename: String): Map[String, Bag] = {
    val colorRegex = """(\d+) ([a-z ]+) bags?.?""".r
    Source
      .fromResource(filename)
      .getLines
      .map(_.split(" bags contain ") match {
        case Array(bagColor, innerBags) =>
          Bag(
            bagColor,
            if (innerBags.contains("no other"))
              Map.empty
            else
              innerBags
                .split(", ")
                .map { case colorRegex(num, clr) =>
                  clr -> num.toInt
                }
                .toMap
          )
      })
      .map(b => (b.color -> b))
      .toMap
  }

  def bfs(root: Bag, bags: Map[String, Bag], color: String): Int = {
    val q = mutable.Queue[Bag](root)
    while (q.nonEmpty) {
      val bag = q.dequeue()
      if (bag.color == color && bag != root)
        return 1

      bag.innerBags.foreach { case (c, _) =>
        bags.get(c) match {
          case Some(innerBag) => q.enqueue(innerBag)
        }
      }
    }
    0
  }

  def part1(input: Map[String, Bag]): Int = {
    input.values.map(r => bfs(r, input, "shiny gold")).sum
  }

  def bfs2(root: Bag, bags: Map[String, Bag]): Int = {
    val q = mutable.Queue[(Bag, Int)](root -> 1)
    var total: Int = 0
    while (q.nonEmpty) {
      val (bag, count) = q.dequeue()
      if (bag != root)
        total += count

      bag.innerBags.foreach { case (clr: String, n: Int) =>
        bags.get(clr) match {
          case Some(innerBag) =>
            q.enqueue(innerBag -> n * count)
        }
      }
    }
    total
  }

  def part2(input: Map[String, Bag]): Int = {
    bfs2(input.get("shiny gold").head, input)
  }

  val input = readFile("input_day7")
  println(s"Part1: ${part1(input)}")

  val sample = readFile("sample_day7")
  assert(part2(sample) == 126)

  println(s"Part2: ${part2(input)}")
}
