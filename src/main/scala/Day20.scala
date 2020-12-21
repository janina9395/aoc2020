import scala.io.Source

case class Tile(number: Int, data: Seq[Seq[Char]]) {
  val top: Seq[Char] = data.head
  val bottom: Seq[Char] = data.last
  val left: Seq[Char] = data.map(_.head)
  val right: Seq[Char] = data.map(_.last)

  def linesUpLeft(tile: Tile): Boolean =
    tile.number != number && left == tile.right
  def linesUpRight(tile: Tile): Boolean =
    tile.number != number && right == tile.left
  def linesUpBottom(tile: Tile): Boolean =
    tile.number != number && bottom == tile.top
  def linesUpTop(tile: Tile): Boolean =
    tile.number != number && top == tile.bottom

  def flip: Tile = {
    Tile(number, data.map(_.reverse))
  }

  def rotate: Tile = {
    Tile(number, data.transpose.reverse)
  }

  def edges: Set[Seq[Char]] = {
    Set(left, right, bottom, top)
  }

  def allPositions: Seq[Tile] = {
    Seq(
      this,
      rotate,
      rotate.rotate,
      rotate.rotate.rotate,
      flip,
      flip.rotate,
      flip.rotate.rotate,
      flip.rotate.rotate.rotate
    )
  }

  def trimBorders: Tile = {
    Tile(
      number,
      data.slice(1, data.length - 2).map(_.slice(0, data.length - 2))
    )
  }
}

object Day20 extends App {

  private val tileParser: Array[String] => Tile = { lines =>
    val tileReg = """Tile (\d+):""".r

    lines.toList match {
      case name :: data =>
        val number = name match {
          case tileReg(num) => num.toInt
        }

        val image =
          data.map(_.trim).filterNot(_.isEmpty).map(_.toCharArray.toSeq)
        Tile(number, image)
    }
  }

  def readFile(filename: String): Seq[Tile] =
    Source
      .fromResource(filename)
      .getLines
      .map(_.trim)
      .mkString("\n")
      .split("\n\n")
      .map(s => tileParser(s.split("\n")))

  def findCorners(tiles: Seq[Tile]) = {
    tiles
      .map { tile =>
        val tileEdges = tile.edges ++ tile.edges.map(_.reverse)
        val otherEdges =
          tiles.filterNot(_.number == tile.number).flatMap(_.edges).toSet

        tile -> (otherEdges intersect tileEdges).size
      }
      .filter(_._2 == 2)
      .map(_._1.number.toLong)
  }

  def part1(tiles: Seq[Tile]): Long = {
    val corners = findCorners(tiles)
    corners.product
  }

  def findNext(
      left: Option[Tile],
      up: Option[Tile],
      available: Set[Tile]
  ): Option[Tile] = {
    available
      .flatMap(
        _.allPositions.find(pos =>
          (left.isEmpty || pos.linesUpLeft(left.get))
            && (up.isEmpty || pos.linesUpTop(up.get))
        )
      )
      .headOption
  }

  def arrange(tiles: Seq[Tile]) = {

    val corners = findCorners(tiles)

    var start = tiles.find(_.number == corners.head).get
    val dim = Math.sqrt(tiles.length).toInt
    val image = Array.ofDim[Tile](dim, dim)

    var availableTiles = tiles.toSet
    availableTiles = availableTiles - start

    start = start.allPositions
      .find(t =>
        findNext(left = Some(t), up = None, availableTiles).isDefined &&
          findNext(left = None, up = Some(t), availableTiles).isDefined
      )
      .get

    for (i <- 0 until dim; j <- 0 until dim) {
      if (j == 0) {
        if (i == 0)
          image(i)(j) = start
        else {
          image(i)(j) =
            findNext(None, Some(image(i - 1)(j)), availableTiles).get
        }
      } else {
        if (i == 0) {
          image(i)(j) =
            findNext(Some(image(i)(j - 1)), None, availableTiles).get
        } else {
          image(i)(j) = findNext(
            Some(image(i)(j - 1)),
            Some(image(i - 1)(j)),
            availableTiles
          ).get
        }
      }
      availableTiles =
        availableTiles - tiles.find(_.number == image(i)(j).number).get
    }

    image.flatMap { rowTiles =>
      val row = rowTiles.map(_.trimBorders)
      val merged =
        row.head.data.indices.map(i => row.flatMap(_.data(i)).mkString(""))
//      println(merged.mkString("\n"))
      merged
    }
  }

  // Fix me
  def part2(tiles: Seq[Tile]) = {
    val image = arrange(tiles)
    val monster = """                  # 
                    |#    ##    ##    ###
                    | #  #  #  #  #  #   """.stripMargin
      .split("\n")
      .map(_.toCharArray)

    val monsterPattern = monster.map(_.count(_ == '#')).sum

    Tile(0, image.map(_.toCharArray.toSeq).toSeq).allPositions
      .map { im =>
        var count = 0
        val data = im.data.map(_.toArray).toArray

        for (
          i <- 0 to im.data.length - monster.length;
          j <- 0 to im.data.head.length - monster(0).length
        ) {
          var matching = 0
          for (mr <- monster.indices; mc <- monster(0).indices) {
            if (
              monster(mr)(mc) == '#' &&
              im.data(i + mr)(j + mc) == monster(mr)(mc)
            ) {
              matching += 1
              data(i + mr)(j + mc) = 'O'
            }
          }
          //println(s"Matching $matching")

          if (matching == monsterPattern)
            count += 1
        }

//        println(s"Debug: $count -> ${data.map(_.count(_ == '#')).sum}")
        count -> data.map(_.count(_ == '#')).sum
      }
      .find { case (count, _) =>
        count == 2
      }
      .map { case (_, res) =>
        res
      }
  }

  val sample: Seq[Tile] = readFile("sample_day20")
  val tiles: Seq[Tile] = readFile("input_day20")

  assert(part1(sample) == 20899048083289L)
  println(s"Part 1 answer: ${part1(tiles)}")

  println(part2(sample))
  //assert(part2(sample).contains(273))
  println(s"Part 2 answer: ${part2(tiles)}")

}
