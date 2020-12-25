
object Day25 extends App {

  def operation(value: Long, subjectNumber: Int): Long = {
    value * subjectNumber % 20201227
  }

  def findLoop(pubKey: Int): Int = {
    var cur = 1L
    var i = 0
    do {
      cur = operation(cur, 7)
      i += 1
    } while (cur != pubKey)
    i
  }

  def encKey(pubKey: Int, loop: Int): Long = {
    var res = 1L
    for (_ <- 0 until loop) {
      res = operation(res, pubKey)
    }
    res
  }


  def part1(num1: Int, num2: Int): Long = {
    val l1 = findLoop(num1)
    val l2 = findLoop(num2)

    val cur = encKey(num2, l1)
    val cur2 = encKey(num1, l2)
    assert(cur == cur2)
    cur
  }

  assert(part1(5764801, 17807724) == 14897079)
  println(s"Part 1: ${part1(11562782, 18108497)}")

}
