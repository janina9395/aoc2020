import scala.annotation.tailrec

object Day15 extends App {

  def lastSpoken(in: Seq[Int], n: Int = 2020): Int = {
    var numbersSpoken = in.slice(0, in.length - 1).zipWithIndex.toMap
    var lastNum = in.last

    for (index <- in.length to n) {
      val nextLastNum =
        numbersSpoken.get(lastNum).map(index - 1 - _).getOrElse(0)
      numbersSpoken += lastNum -> (index - 1)
      if (index < n)
        lastNum = nextLastNum
    }
    lastNum
  }

  // Recursive version to compare performance
  def lastSpokenRec(in: Seq[Int], n: Int = 2020): Int = {

    @tailrec
    def f(lastnum: Int, index: Int, numberIndex: Map[Int, Int]): Int = {
      if (index == n) {
        lastnum
      } else {
        val nextLastNum =
          numberIndex.get(lastnum).map(index - 1 - _).getOrElse(0)
        f(nextLastNum, index + 1, numberIndex + (lastnum -> (index - 1)))
      }
    }

    f(in.last, in.length, in.slice(0, in.length - 1).zipWithIndex.toMap)
  }

  assert(lastSpokenRec(Seq(0, 3, 6)) == 436)
  assert(lastSpokenRec(Seq(1, 3, 2)) == 1)
  assert(lastSpokenRec(Seq(2, 1, 3)) == 10)
  assert(lastSpokenRec(Seq(1, 2, 3)) == 27)
  assert(lastSpokenRec(Seq(2, 3, 1)) == 78)
  assert(lastSpokenRec(Seq(3, 2, 1)) == 438)
  assert(lastSpokenRec(Seq(3, 1, 2)) == 1836)

  println(s"Part1: ${lastSpokenRec(Seq(2, 20, 0, 4, 1, 17))}")

  //assert(LastSlokenRec
  //(Seq(0, 3, 6), 30000000) == 175594)
  println(s"Part2: ${lastSpokenRec(Seq(2, 20, 0, 4, 1, 17), 30000000)}")
}
