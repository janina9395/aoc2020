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

  assert(lastSpoken(Seq(0, 3, 6)) == 436)
  assert(lastSpoken(Seq(1, 3, 2)) == 1)
  assert(lastSpoken(Seq(2, 1, 3)) == 10)
  assert(lastSpoken(Seq(1, 2, 3)) == 27)
  assert(lastSpoken(Seq(2, 3, 1)) == 78)
  assert(lastSpoken(Seq(3, 2, 1)) == 438)
  assert(lastSpoken(Seq(3, 1, 2)) == 1836)

  println(s"Part1: ${lastSpoken(Seq(2, 20, 0, 4, 1, 17))}")

  //assert(lastSpoken(Seq(0, 3, 6), 30000000) == 175594)
  println(s"Part2: ${lastSpoken(Seq(2, 20, 0, 4, 1, 17), 30000000)}")
}
