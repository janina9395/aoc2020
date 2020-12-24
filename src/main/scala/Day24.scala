case class Pos(x: Int, y: Int, z: Int) {
  def + (pos: Pos): Pos = {
    Pos(x + pos.x, y + pos.y, z + pos.z)
  }
}

object Day24 extends App {

  implicit val parser = new Parser[Seq[String]] {
    private val reg = "e|se|sw|w|nw|ne".r
    override def parse(string: String): Seq[String] = {
      reg.findAllIn(string).toSeq
    }
  }

  def part1(tileDirections: Seq[Seq[String]]): Int = {
    // from https://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/AV0405/MARTIN/Hex.pdf
    val step = Map(
      "e" -> Pos(-1, -1, 0),
      "se" -> Pos(0, -1, -1),
      "sw" -> Pos(1, 0, -1),
      "w" -> Pos(1, 1, 0),
      "nw" -> Pos(0, 1, 1),
      "ne" -> Pos(-1, 0, 1)
    )

    val start = Pos(0, 0, 0)
    val tiles = tileDirections
      .map ( directions =>
        directions.map(dir => step(dir))
          .foldLeft(start)(_ + _)
      )

    tiles
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count(_._2 % 2 == 1)
  }

  def part2(directions: Seq[Seq[String]]): Int = {
    ???
  }


  val sample = FileReader.read[Seq[String]]("sample_day24")
  assert(part1(sample) == 10)

  val input = FileReader.read[Seq[String]]("input_day24")
  println(s"Part 1 answer: ${part1(input)}")

  //assert(part2(sample) == 2208)
  println(s"Part 2 answer: ${part2(input)}")

}
