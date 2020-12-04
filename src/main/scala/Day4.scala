import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class Passport(
    byr: Option[Int],
    iyr: Option[Int],
    eyr: Option[Int],
    hgt: Option[String],
    hcl: Option[String],
    ecl: Option[String],
    pid: Option[String],
    cid: Option[String]
) {

  def isValid: Boolean = {
    byr.nonEmpty &&
    iyr.nonEmpty &&
    eyr.nonEmpty &&
    hgt.nonEmpty &&
    hcl.nonEmpty &&
    ecl.nonEmpty &&
    pid.nonEmpty
  }

  def isValidExt: Boolean = {
    import Passport._
    isValid &&
    validByrRange.contains(byr.get) &&
    validIyrRange.contains(iyr.get) &&
    validEyrRange.contains(eyr.get) &&
    hgt.get.matches(hgtRegex) &&
    hcl.get.matches(hclRegex) &&
    validEcl.contains(ecl.get) &&
    pid.get.matches(pidRegex)
  }
}

object Passport {
  val validByrRange: Range.Inclusive = 1920 to 2002
  val validIyrRange: Range.Inclusive = 2010 to 2020
  val validEyrRange: Range.Inclusive = 2020 to 2030
  val hgtRegex = "1([5-8][0-9]|9[0-3])cm|([5-6][0-9]|7[0-6])in"
  val hclRegex = "#[0-9a-f]{6}"
  val pidRegex = "[0-9]{9}"
  val validEcl = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def apply(fields: Map[String, String]): Passport = {
    Passport(
      byr = fields.get("byr").map(_.toInt),
      iyr = fields.get("iyr").map(_.toInt),
      eyr = fields.get("eyr").map(_.toInt),
      hgt = fields.get("hgt"),
      hcl = fields.get("hcl"),
      ecl = fields.get("ecl"),
      pid = fields.get("pid"),
      cid = fields.get("cid")
    )
  }
}

object Day4 extends App {

  def readFile(filename: String): Seq[Passport] = {
    var res = ArrayBuffer[Passport]()
    var fields = Map[String, String]()
    Source.fromResource(filename).getLines().foreach { l =>
      if (l.isEmpty) {
        res += Passport(fields)
        fields = Map.empty
      } else {
        fields ++= l
          .split("[ \n]")
          .map(_.split(":"))
          .map { case Array(k, v) => k -> v }
          .toMap

      }
    }
    res += Passport(fields)
    res.toSeq
  }

  val sample = readFile("sample_day4")
  assert(sample(0).isValid)
  assert(!sample(1).isValid)
  assert(sample(2).isValid)
  assert(!sample(3).isValid)

  val input = readFile("input_day4")
  println(s"Part1: ${input.count(_.isValid)}")

  val sample2 = readFile("sample2_day4")
  assert(sample2.count(_.isValidExt) == 4)
  println(s"Part2: ${input.count(_.isValidExt)}")

}
