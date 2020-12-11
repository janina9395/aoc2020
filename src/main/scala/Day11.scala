import scala.io.Source

class PlainGrid(val input: Seq[Array[Char]]) {

  private def isValidIndex(i: Int, j: Int): Boolean = {
    i >= 0 && j >= 0 && i < input.length && j < input.head.length
  }

  def adjSeats(i: Int, j: Int): Seq[Char] = {
    for {
      r <- i - 1 to i + 1
      c <- j - 1 to j + 1 if isValidIndex(r, c) && !(r == i && c == j)
    } yield input(r)(c)
  }

  def countBusy: Int = {
    input.map(_.count(_ == '#')).sum
  }

  private def findVisibleSeat(i: Int, j: Int, dx: Int, dy: Int): Char = {
    var c = '.'
    var row = i + dy
    var col = j + dx
    while (isValidIndex(row, col) && c == '.') {
      c = input(row)(col)
      row += dy
      col += dx
    }
    c
  }

  def visibleSeats(i: Int, j: Int): Seq[Char] = {
    for {
      dy <- -1 to 1
      dx <- -1 to 1 if !(dx == 0 && dy == 0)
    } yield findVisibleSeat(i, j, dx, dy)
  }

  def round(
      seatFunc: (Int, Int) => Seq[Char],
      busyLimit: Int
  ): Option[PlainGrid] = {
    val output = Array.ofDim[Char](input.size, input.head.length)
    var updated = false

    for (i <- input.indices; j <- input.head.indices) {
      if (input(i)(j) == 'L' && seatFunc(i, j).count(_ == '#') == 0) {
        output(i)(j) = '#'
        updated = true
      } else if (
        input(i)(j) == '#' && seatFunc(i, j).count(_ == '#') >= busyLimit
      ) {
        output(i)(j) = 'L'
        updated = true
      } else
        output(i)(j) = input(i)(j)
    }

    if (updated) Some(new PlainGrid(output)) else None
  }
}

object Day11 extends App {

  private def readFile(filename: String): PlainGrid = {
    new PlainGrid(
      Source
        .fromResource(filename)
        .getLines
        .map(_.toCharArray)
        .toSeq
    )
  }

  def run(input: PlainGrid, roundFunc: PlainGrid => Option[PlainGrid]): Int = {
    var in = input
    var updated = true
    do {
      roundFunc(in) match {
        case Some(out) =>
          updated = true
          in = out
        case None =>
          updated = false
      }
    } while (updated)
    in.countBusy
  }

  def part1(input: PlainGrid): Int = {
    run(input, in => in.round(in.adjSeats, 4))
  }

  def part2(input: PlainGrid): Int = {
    run(input, in => in.round(in.visibleSeats, 5))
  }

  val sample = readFile("sample_day11")
  assert(part1(sample) == 37)

  val input = readFile("input_day11")
  println(s"Part1: ${part1(input)}")

  assert(part2(sample) == 26)
  println(s"Part2: ${part2(input)}")
}
