import scala.collection.mutable
import scala.io.Source

case class Instruction(index: Int, value: Int, mask: Array[Char])

object Instruction {

  def binaryStringWithLeadingZeros(number: Int): String =
    f"${number.toBinaryString}%36s".replace(' ', '0')

  def applyMask(value: Int, mask: Array[Char]): Long = {
    java.lang.Long.valueOf(binaryStringWithLeadingZeros(value).toCharArray.zip(mask).map {
      case (n, 'X') => n
      case (_, m) => m
    }.mkString(""), 2)
  }

  def applyMaskToAddress(value: Int, mask: Array[Char]): Seq[Long] = {

    def replaceX(in: String, repl: String): Seq[String] = {
      if (!in.contains('X')) {
        Seq(in)
      } else {
        val updated = in.replaceFirst("X", repl)
        replaceX(updated, "0") ++ replaceX(updated, "1")
      }
    }

    val s = binaryStringWithLeadingZeros(value).toCharArray.zip(mask).map {
      case (_, 'X') => 'X'
      case (n, '0') => n
      case (_, '1') => '1'
    }.mkString("")

    (replaceX(s, "0") ++ replaceX(s, "1")).map(java.lang.Long.valueOf(_, 2))
  }
}

object Day14 extends App {

  private def readFile(filename: String): Seq[Instruction] = {
    val maskRegex = """mask = (\w+)""".r
    val memRegex = """mem\[(\d+)\] = (\d+)""".r

    var mask = Array[Char]()
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .flatMap {
      case maskRegex(n) =>
        mask = n.toCharArray
        None
      case memRegex(index, value) =>
        Some(Instruction(index.toInt, value.toInt, mask))
    }

  }

  def part1(input: Seq[Instruction]) = {
    var res = mutable.Map[Int, Long]()
    input.foreach { instr =>
      res += instr.index -> Instruction.applyMask(instr.value, instr.mask)
    }
    res.values.sum
  }

  def part2(input: Seq[Instruction]) = {
    var res = mutable.Map[Long, Long]()
    input.foreach { instr =>
      Instruction.applyMaskToAddress(instr.index, instr.mask).foreach { address =>
        res += address -> instr.value.toLong
      }
    }
    res.values.sum
  }

  val sample = readFile("sample_day14")
  assert(part1(sample) == 165)

  val input = readFile("input_day14")
  println(s"Part1: ${part1(input)}")

  val sample2 = readFile("sample_day14_2")
  assert(part2(sample2) == 208)
  println(s"Part2: ${part2(input)}")
}
