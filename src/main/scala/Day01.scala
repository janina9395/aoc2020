import scala.io.Source

object Day01 extends App {

  def readFile(filename: String): Seq[Int] =
    Source.fromResource(filename).getLines.map(_.toInt).toSeq

  def findProductOf2(items: Seq[Int], expectedSum: Int): Option[Long] = {
    for (i <- items.indices; j <- i + 1 until items.size)
      if (items(i) + items(j) == expectedSum)
        return Some(items(i) * items(j))
    None
  }

  def findProductOf3(items: Seq[Int], expectedSum: Int): Option[Long] = {
    for (
      i <- items.indices;
      j <- i + 1 until items.size if items(i) + items(j) < expectedSum;
      k <- j + 1 until items.size
    ) {
      if (items(i) + items(j) + items(k) == expectedSum)
        return Some(items(i) * items(j) * items(k))
    }
    None
  }

  val sample = Seq(1721, 979, 366, 299, 675, 1456)
  assert(findProductOf2(sample, 2020).contains(514579))
  assert(findProductOf3(sample, 2020).contains(241861950))

  val items: Seq[Int] = readFile("input_day1")
  println(s"Part 1 answer:${findProductOf2(items, 2020)}")
  println(s"Part 2 answer:${findProductOf3(items, 2020)}")

}
