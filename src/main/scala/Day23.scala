import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Day23 extends App {

  def run(items: Seq[Int], moves: Int) = {
    def index(ind: Int, length: Int) = {
      if (ind >= length) ind % length else ind
    }

    def dst(cur: Int, items: ArrayBuffer[Int], dropped: Seq[Int]): Int = {
      var min = 1
      var max = items.length + dropped.length

      while(dropped.contains(min)) min += 1
      while(dropped.contains(max)) max -= 1

      var t = cur - 1
      if (t < min) t = max
      while (dropped.contains(t)) {
        t -= 1
        if (t < min)
          t = max
      }
      t
    }

    def move(cur: Int, items: ArrayBuffer[Int]) = {
      val curIndex = index(items.indexOf(cur), items.length)

      val dropped = (1 to 3).map(i => items(index(curIndex + i, items.length)))
      dropped.foreach { item =>
        items.remove(items.indexOf(item))
      }

//      val len = items.length
//      val dropped = (1 to 3).map(i =>
//        items.remove(index(curIndex + 1, len + 1 - i))
//      )

      //println(s"Pick up: ${dropped.mkString(", ")}")
      val dest = dst(cur, items, dropped)
      //println(s"Destination: $dest")
      val dstIndex = items.indexOf(dest)

      val (prefix, suffix) = items.splitAt(dstIndex)

      suffix.remove(0)
      (prefix :+ dest) ++ dropped ++ suffix
    }

    var it = ArrayBuffer.from(items)
    var cur = it.head
    (0 until moves).foreach { i =>
      println(s"-- Move ${i + 1} --")
      //println(s"Cups: ${it.mkString(", ")}")
      //println(s"Current: $cur")
      it = move(cur, it)
      cur = it(index(it.indexOf(cur) + 1, it.length))
    }
    //println(s"Final: ${it.mkString(",")}")
    it
  }

  def part1(items: Seq[Int], moves: Int = 100): String = {

    def result(items: ArrayBuffer[Int]): String = {
      val (prefix, suffix) = items.splitAt(items.indexOf(1))
      suffix.remove(0)
      (suffix ++ prefix).mkString("")
    }

    val res = run(items, moves)
    result(res)
  }


  def part2(items: Seq[Int]): Long = {
    val res = run(items ++ (items.length to 1_000_000), moves = 10_000_000)
    val i = res.indexOf(1)

    res((i + 1) % res.length).toLong * res((i + 2) % res.length).toLong

  }

  def parseAsInts(input: String): Seq[Int] = {
    input.toCharArray.map(_ - '0').toSeq
  }

  val sample = parseAsInts("389125467")
  println(sample)
  assert(part1(sample, moves = 10) == "92658374")
  assert(part1(sample, moves = 100) == "67384529")

  val input = parseAsInts("253149867")
  println(s"Part 1 answer: ${part1(input)}")

  //assert(part2(sample) == 149245887792L)
  println(s"Part 2 answer: ${part2(input)}")

}
