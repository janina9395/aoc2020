import scala.io.Source

trait Mapper[A] {
  def map(line: String): A
}

object FileReader {

  def readLines(fileName: String): Seq[String] = {
    read[String](fileName)(identity)
  }

  def read[A: Mapper](filename: String): Seq[A] = {
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .map(implicitly[Mapper[A]].map(_))
  }
}
