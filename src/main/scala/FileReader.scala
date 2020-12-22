import scala.io.Source

trait Parser[A] {
  def parse(string: String): A
}

object FileReader {

  def readLines(fileName: String): Seq[String] = {
    read[String](fileName)(identity)
  }

  def readBlocks(fileName: String): Seq[String] = {
    Source
      .fromResource(fileName)
      .getLines
      .map(_.trim)
      .mkString("\n")
      .split("\n\n")
  }

  def read[A: Parser](filename: String): Seq[A] = {
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .map(implicitly[Parser[A]].parse(_))
  }
}
