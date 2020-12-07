import scala.collection.mutable
import scala.io.Source

case class Bag(color: String, innerBags: Map[String, Int])

object Day7 extends App {

  def readFile(filename: String): Seq[Bag] = {
    val colorRegex = """(\d+) ([a-z ]+) bags?.?""".r

    Source
      .fromResource(filename)
      .getLines
      .map(_.split("contain") match {
        case Array(bagColor, innerBags) =>
          Bag(
            bagColor.replace("bags", "").trim,
            if (innerBags.contains("no other"))
              Map.empty
            else
              innerBags
                .split(",")
                .map { s =>
                  s.trim match {
                    case colorRegex(num, clr) =>
                      clr -> num.toInt
                  }
                }
                .toMap
          )

      })
      .toSeq
  }

  def bfs(root: Bag): Int = {
    val q = mutable.Queue[Bag](root)
    while (q.nonEmpty) {
      val bag = q.dequeue()
      if (bag.color == "shiny gold" && bag != root)
        return 1

      bag.innerBags.foreach { case (c, _) =>
        input.find(_.color == c) match {
          case Some(innerBag) => q.enqueue(innerBag)
        }
      }
    }
    0
  }

  def part1(input: Seq[Bag]): Int = {
    input.map(bfs).sum
  }

  def bfs2(root: Bag, bags: Seq[Bag]): Int = {
    val q = mutable.Queue[(Bag, Int)](root -> 1)
    var total: Int = 0
    while (q.nonEmpty) {
      val (bag, count) = q.dequeue()
      if (bag != root)
        total += count

      bag.innerBags.foreach { case (clr: String, n: Int) =>
        bags.find(_.color == clr) match {
          case Some(innerBag) =>
            q.enqueue(innerBag -> n * count)
        }
      }
    }
    total
  }

  def part2(input: Seq[Bag]): Int = {
    bfs2(input.find(_.color == "shiny gold").head, input)
  }

  val input = readFile("input_day7")
  println(s"Part1: ${part1(input)}")

  val sample = readFile("sample_day7")
  assert(part2(sample) == 126)

  println(s"Part2: ${part2(input)}")
}
