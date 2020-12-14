import scala.io.Source

object StringImplicits {
  implicit class StringBinaryOps(s: String) {

    def leftPadTo(length: Int, ch: Char): String = {
      "".padTo(length - s.length, ch) + s
    }

    def toLongRadix(radix: Int): Long = {
      java.lang.Long.valueOf(s, radix)
    }

    def toIntRadix(radix: Int): Int = {
      Integer.valueOf(s, radix)
    }
  }
}

case class Instruction(index: Int, value: Int, mask: Array[Char])

object Instruction {
  import StringImplicits._

  def applyMask(value: Int, mask: Array[Char]): Long = {
    value.toBinaryString
      .leftPadTo(36, '0')
      .toCharArray
      .zip(mask)
      .map {
        case (n, 'X') => n
        case (_, m)   => m
      }
      .mkString("")
      .toLongRadix(2)
  }

  def applyMaskToAddress(value: Int, mask: Array[Char]): Seq[Long] = {

    def allXReplaced(s: String): Seq[String] = {
      def replaceX(in: String, replacement: String): Seq[String] = {
        if (!in.contains('X')) {
          Seq(in)
        } else {
          val updated = in.replaceFirst("X", replacement)
          replaceX(updated, "0") ++ replaceX(updated, "1")
        }
      }

      replaceX(s, "0") ++ replaceX(s, "1")
    }

    val s = value.toBinaryString
      .leftPadTo(36, '0')
      .toCharArray
      .zip(mask)
      .map {
        case (_, 'X') => 'X'
        case (n, '0') => n
        case (_, '1') => '1'
      }
      .mkString("")

    allXReplaced(s).map(_.toLongRadix(2))
  }
}

object Day14 extends App {
  import Instruction._

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

  def part1(input: Seq[Instruction]): Long = {
    input
      .map { instr =>
        instr.index -> applyMask(instr.value, instr.mask)
      }
      .toMap
      .values
      .sum
  }

  def part2(input: Seq[Instruction]): Long = {
    input
      .flatMap { instr =>
        applyMaskToAddress(instr.index, instr.mask).map { address =>
          address -> instr.value.toLong
        }
      }
      .toMap
      .values
      .sum
  }

  val sample = readFile("sample_day14")
  assert(part1(sample) == 165)

  val input = readFile("input_day14")
  println(s"Part1: ${part1(input)}")

  val sample2 = readFile("sample_day14_2")
  assert(part2(sample2) == 208)
  println(s"Part2: ${part2(input)}")
}
