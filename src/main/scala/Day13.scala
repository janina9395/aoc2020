import scala.io.Source

object Day13 extends App {

  private def readFile(filename: String): (Int, Seq[Int]) = {
    Source
      .fromResource(filename)
      .getLines
      .toList match {
      case ts :: buses :: Nil =>
        ts.toInt -> buses.split(",").filterNot(_ == "x").map(_.toInt).toSeq
    }
  }

  private def readFilePart2(filename: String): Seq[(Int, Int)] = {
    Source
      .fromResource(filename)
      .getLines
      .toList match {
      case _ :: buses :: Nil =>
        buses
          .split(",")
          .zipWithIndex
          .filterNot(_._1 == "x")
          .map { case (n, ind) => (n.toInt, ind) }
          .toSeq
    }
  }

  def part1(input: String): Int = {
    val (ts, buses) = readFile(input)

    def findBus(depTime: Int): Option[Int] = {
      buses.find(busDep => depTime % busDep == 0)
    }

    var depTime = ts - 1
    var bus: Option[Int] = None
    while (bus.isEmpty) {
      depTime += 1
      bus = findBus(depTime)
    }

    (depTime - ts) * bus.get
  }

  def part2(input: String, start: Long): Long = {
    val buses = readFilePart2(input)
    var ts: Long = start

    var found = false
    var step = 1L
    while (!found) {
      ts += step
      val bussesAligned = buses.takeWhile { case (bus, offset) =>
        (ts + offset) % bus == 0
      }
      found = bussesAligned.length == buses.length

      // Step is Least common multiple of aligned busses
      // In our case it is their product
      step = bussesAligned.map { case (bus, _) =>
        bus.toLong
      }.product
    }
    ts
  }

  assert(part1("sample_day13") == 295)
  println(s"Part1: ${part1("input_day13")}")

  assert(part2("sample_day13", 0L) == 1068781)
  println(s"Part2: ${part2("input_day13", 100000000000000L)}")
}
