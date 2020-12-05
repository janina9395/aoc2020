import scala.io.Source

object Day5 extends App {

  def decodeSeat(code: Array[Char]): Int = {
    var i = 0

    def search(fChar: Char, bChar: Char, end: Int): Int = {
      var b = 0
      var e = end

      while (b != e) {
        val step = (e - b) / 2
        if (code(i) == fChar) {
          e = e - step - 1
        } else if (code(i) == bChar) {
          b = b + step + 1
        }
        i = i + 1
      }
      b
    }

    val row = search('F', 'B', 127)
    val col = search('L', 'R', 7)
    row * 8 + col
  }

  def findSeat(seats: Seq[Int]): Option[Int] = {
    val sorted = seats.sorted
    sorted.zipWithIndex.foreach { case (i: Int, _: Int) =>
      if (i + 1 < sorted.length && sorted(i + 1) - sorted(i) > 1)
        return Some(sorted(i + 1) - 1)
    }
    None
  }

  def readFile(filename: String): Seq[Array[Char]] =
    Source.fromResource(filename).getLines.map(_.trim.toCharArray).toSeq

  assert(decodeSeat("FBFBBFFRLR".toCharArray) == 357)

  val input = readFile("input_day5")
  val seats = input.map(decodeSeat)
  println(s"Part1: ${seats.max}")
  println(s"Suspected seat is ${findSeat(seats).get}")
}
