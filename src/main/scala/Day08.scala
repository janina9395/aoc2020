import scala.io.Source

case class Command(op: String, arg: Int)
class Program(commands: Seq[Command]) {
  private var acc = 0
  private var i = 0
  private var indexes = Set[Int]()

  def inLoop: Boolean = {
    indexes.contains(i)
  }

  def execute(): Int = {
    while (i < commands.size && !inLoop) {
      indexes += i
      commands(i) match {
        case Command("nop", _) =>
          i += 1
        case Command("acc", arg) =>
          acc += arg
          i += 1
        case Command("jmp", arg) =>
          i += arg
      }
    }
    acc
  }
}

object Day8 extends App {

  private def readFile(filename: String): Seq[Command] = {
    val cmdRegex = """([a-z]{3}) (-?\+?\d+)""".r
    Source
      .fromResource(filename)
      .getLines
      .map { case cmdRegex(cmd, arg) =>
        Command(cmd, arg.toInt)
      }
      .toSeq
  }

  def part1(input: Seq[Command]): Int = {
    new Program(input).execute()
  }

  private def execute(input: Seq[Command]): Option[Int] = {
    val program = new Program(input)
    val acc = program.execute()
    if (program.inLoop) {
      None
    } else {
      Some(acc)
    }
  }

  def part2(input: Seq[Command]): Int = {
    input.zipWithIndex
      .map { case (cmd, i) =>
        if (cmd.op == "nop" && cmd.arg != 0) {
          execute(input.updated(i, Command("jmp", cmd.arg)))
        } else if (cmd.op == "jmp") {
          execute(input.updated(i, Command("nop", cmd.arg)))
        } else None
      }
      .find(_.isDefined)
      .flatten
      .get
  }

  val sample = readFile("sample_day8")
  println(sample)
  assert(part1(sample) == 5)

  val input = readFile("input_day8")
  println(s"Part1: ${part1(input)}")

  assert(part2(sample) == 8)
  println(s"Part2: ${part2(input)}")
}
