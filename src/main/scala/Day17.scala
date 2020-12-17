import scala.io.Source

case class Point3D(x: Int, y: Int, z: Int) {

  def neighbours: Seq[Point3D] = {
    for {
      i <- x - 1 to x + 1
      j <- y - 1 to y + 1
      k <- z - 1 to z + 1 if !(i == x && j == y && k == z)
    } yield Point3D(i, j, k)
  }
}

case class Point4D(x: Int, y: Int, z: Int, w: Int) {

  def neighbours: Seq[Point4D] = {
    for {
      i <- x - 1 to x + 1
      j <- y - 1 to y + 1
      k <- z - 1 to z + 1
      l <- w - 1 to w + 1 if !(i == x && j == y && k == z && l == w)
    } yield Point4D(i, j, k, l)
  }
}


object Day17 extends App {

  private def readFile(filename: String): Map[Point3D, Char] = {
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .zipWithIndex
      .flatMap { case (line, row) =>
        line.toCharArray.zipWithIndex.map { case (ch, col) =>
          Point3D(col, row, 0) -> ch
        }
      }.toMap.withDefaultValue('.')
  }

  private def readFilePart2(filename: String): Map[Point4D, Char] = {
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .zipWithIndex
      .flatMap { case (line, row) =>
        line.toCharArray.zipWithIndex.map { case (ch, col) =>
          Point4D(col, row, 0, 0) -> ch
        }
      }.toMap.withDefaultValue('.')
  }

  private def nextState(curState: Char, activeNeighbours: Int): Char = {
    curState match {
      case '#' =>
        if (activeNeighbours == 3 || activeNeighbours == 2)
          '#'
        else
          '.'
      case '.' =>
        if (activeNeighbours == 3) {
          '#'
        }
        else
          '.'
    }
  }


  def part1(filename: String): Int = {
    val initState = readFile(filename)

    def round(state: Map[Point3D, Char]): Map[Point3D, Char] = {
      val points = state.keys
      val (minX, minY, minZ) = (points.map(_.x).min, points.map(_.y).min, points.map(_.z).min)
      val (maxX, maxY, maxZ) = (points.map(_.x).max, points.map(_.y).max, points.map(_.z).max)

      (for {
        i <- minX - 1 to maxX + 1
        j <- minY - 1 to maxY + 1
        k <- minZ - 1 to maxZ + 1
      } yield {
        val p = Point3D(i, j, k)
        val active = p.neighbours.map(state(_)).count(s => s == '#')
        p -> nextState(state(p), active)
      }).toMap.withDefaultValue('.')
    }

    Iterator.iterate(initState, 6)(round)
      .toSeq
      .last
      .values
      .count(_ == '#')
  }

  def part2(filename: String): Int = {
    val initState = readFilePart2(filename)

    def round(state: Map[Point4D, Char]): Map[Point4D, Char] = {
      val points = state.keys
      val (minX, minY, minZ, minW) = (points.map(_.x).min, points.map(_.y).min, points.map(_.z).min, points.map(_.w).min)
      val (maxX, maxY, maxZ, maxW) = (points.map(_.x).max, points.map(_.y).max, points.map(_.z).max, points.map(_.w).max)

      (for {
        i <- minX - 1 to maxX + 1
        j <- minY - 1 to maxY + 1
        k <- minZ - 1 to maxZ + 1
        l <- minW - 1 to maxW + 1
      } yield {
        val p = Point4D(i, j, k, l)
        val active = p.neighbours.map(state(_)).count(s => s == '#')
        p -> nextState(state(p), active)
      }).toMap.withDefaultValue('.')
    }

    Iterator.iterate(initState, 6)(round)
      .toSeq
      .last
      .values
      .count(_ == '#')
  }

  assert(part1("sample_day17") == 112)
  assert(part2("sample_day17") == 848)

  println(s"Part1: ${part1("input_day17")}")
  println(s"Part2: ${part2("input_day17")}")

}
