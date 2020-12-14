import scala.io.Source

case class Point(x: Int, y: Int) {
  def move(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
  def rotateRight90(): Point = Point(y, -x)
  def rotateLeft90(): Point = Point(-y, x)
  def rotate180(): Point = Point(-x, -y)
}

object Day12 extends App {

  private def readFile(filename: String): Seq[(String, Int)] = {
    Source
      .fromResource(filename)
      .getLines
      .map(s => s.splitAt(1))
      .map { case (dir: String, pos: String) =>
        dir -> pos.toInt
      }
      .toSeq
  }

  def part1(input: Seq[(String, Int)]) = {
    var curAngle = 90 // East direction
    val dirByAngle = Map[Int, String](
      0 -> "N",
      90 -> "E",
      180 -> "S",
      270 -> "W"
    )

    var pos = Point(0, 0)

    def move(dir: String, value: Int): Unit = {
      dir match {
        case "S" => pos = pos.move(0, -value)
        case "N" => pos = pos.move(0, value)
        case "E" => pos = pos.move(value, 0)
        case "W" => pos = pos.move(-value, 0)
      }
    }

    input.foreach { case (dir, value) =>
      dir match {
        case "F" =>
          val curDir = dirByAngle(curAngle)
          move(curDir, value)
        case "R" =>
          curAngle = (curAngle + value) % 360
        case "L" =>
          curAngle = curAngle - value
          if (curAngle < 0) curAngle += 360
        case "S" =>
          move("S", value)
        case "N" =>
          move("N", value)
        case "E" =>
          move("E", value)
        case "W" =>
          move("W", value)
      }
    }
    Math.abs(pos.x) + Math.abs(pos.y)
  }

  def part2(input: Seq[(String, Int)]) = {
    var wayPoint = Point(10, 1)
    var pos = Point(0, 0)

    input.foreach {
      case ("F", value) =>
        pos = pos.move(value * wayPoint.x, value * wayPoint.y)
      case ("R", 90) =>
        wayPoint = wayPoint.rotateRight90()
      case ("R", 180) =>
        wayPoint = wayPoint.rotate180()
      case ("R", 270) =>
        wayPoint = wayPoint.rotateLeft90()
      case ("L", 90) =>
        wayPoint = wayPoint.rotateLeft90()
      case ("L", 180) =>
        wayPoint = wayPoint.rotate180()
      case ("L", 270) =>
        wayPoint = wayPoint.rotateRight90()
      case ("S", value) =>
        wayPoint = wayPoint.move(0, -value)
      case ("N", value) =>
        wayPoint = wayPoint.move(0, value)
      case ("E", value) =>
        wayPoint = wayPoint.move(value, 0)
      case ("W", value) =>
        wayPoint = wayPoint.move(-value, 0)
    }
    Math.abs(pos.x) + Math.abs(pos.y)
  }

  val sample = readFile("sample_day12")
  assert(part1(sample) == 25)

  val input = readFile("input_day12")
  println(s"Part1: ${part1(input)}")

  assert(part2(sample) == 286)
  println(s"Part2: ${part2(input)}")
}
