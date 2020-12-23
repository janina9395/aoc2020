class Item(val value: Int, var next: Item)

object Day23 extends App {

  def run(input: Seq[Int], moves: Long): Array[Item] = {

    def findDest(cur: Item, items: Array[Item], dropped: Seq[Item], indexes: Map[Int, Int]): Item = {
      var min = 1
      var max = items.length

      while (dropped.exists(_.value == min)) min += 1
      while (dropped.exists(_.value == max)) max -= 1

      var t = cur.value - 1
      if (t < min) t = max
      while (dropped.exists(_.value == t)) {
        t -= 1
        if (t < min)
          t = max
      }

      items(indexes(t))
    }

    def move(cur: Item, items: Array[Item], indexes: Map[Int, Int]): Unit = {
      val dropped = Seq(cur.next, cur.next.next, cur.next.next.next)
      val dest = findDest(cur, items, dropped, indexes)

      val oldDestNext = dest.next
      val newCurNext = dropped.last.next
      dest.next = dropped.head
      dropped.last.next = oldDestNext
      cur.next = newCurNext
    }

    val items = Array.from(input.map(new Item(_, null)))
    val indexes = items.map(_.value).zipWithIndex.toMap
    for (i <- 0 until items.length - 1) {
        items(i).next = items(i + 1)
    }
    items.last.next = items.head

    var cur = items.head
    (0L until moves).foreach { i =>
      println(s"-- Move ${i + 1} --")
      move(cur, items, indexes)
      cur = cur.next
    }
    items
  }

  def part1(items: Seq[Int], moves: Int): String = {

    def result(items: Array[Item]): String = {
      val start = items.find(_.value == 1).get
      var cur = start.next

      val res = new StringBuilder("")
      do {
        res ++= cur.value.toString
        cur = cur.next
      } while (cur != start)
      res.result()
    }

    val res = run(items, moves)
    result(res)
  }


  def part2(items: Seq[Int]): Long = {
    val res = run(items ++ (items.length + 1 to 1_000_000), moves = 10_000_000L)
    val i = res.find(_.value == 1).get

    i.next.value.toLong * i.next.next.value.toLong
  }

  def parseAsInts(input: String): Seq[Int] = {
    input.toCharArray.map(_ - '0').toSeq
  }

  val sample = parseAsInts("389125467")
  assert(part1(sample, moves = 10) == "92658374")
  assert(part1(sample, moves = 100) == "67384529")

  val input = parseAsInts("253149867")
  println(s"Part 1 answer: ${part1(input, moves = 100)}")

  assert(part2(sample) == 149245887792L)
  println(s"Part 2 answer: ${part2(input)}")

}
