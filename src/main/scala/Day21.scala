import scala.annotation.tailrec
import scala.collection.Set

case class Food(ingredients: Seq[String], allergens: Seq[String])

object Day21 extends App {

  implicit val foodParser: Parser[Food] = new Parser[Food] {
    private val foodReg = """(.*) \(contains (.*)\)""".r

    override def parse(line: String): Food = {
      line.trim match {
        case foodReg(ingredients, allergens) =>
          Food(
            ingredients.split(" ").map(_.trim),
            allergens.split(",").map(_.trim)
          )
      }
    }
  }

  def findAllergenMap(food: Seq[Food]): Map[String, Set[String]] = {
    var allergenMap = Map[String, Set[String]]()

    food
      .flatMap(f => f.allergens.map(a => a -> f.ingredients.toSet))
      .foreach { case (allergen, ingSet) =>
        val newIngSet = allergenMap.get(allergen) match {
          case Some(curIngSet) =>
            curIngSet intersect ingSet
          case None =>
            ingSet
        }
        allergenMap += allergen -> newIngSet
      }

    allergenMap
  }

  def part1(food: Seq[Food]): Int = {
    val allergenToIng = findAllergenMap(food)

    val unsafeIng = allergenToIng.values.flatten.toSet
    val safeIng = food.flatMap(_.ingredients).filterNot(unsafeIng.contains)

    safeIng.size
  }

  def part2(food: Seq[Food]): String = {

    @tailrec
    def resolve(
        allergenMap: Map[String, Set[String]],
        resolved: Map[String, String]
    ): Map[String, String] = {

      // find 1 to 1 mapping between allergen and ingredient
      def findNextResolved(): Option[(String, String)] = {
        allergenMap
          .find { case (aller, ingSet) =>
            ingSet.size == 1 && !resolved.contains(aller)
          }
          .map { case (aller, ingSet) =>
            aller -> ingSet.head
          }
      }

      findNextResolved() match {
        case Some((aller, ing)) =>
          // exclude resolved ingredient from all other allergen lists
          val updated = allergenMap.map { case (aller, ings) =>
            aller -> (ings - ing)
          }

          resolve(updated, resolved + (ing -> aller))
        case None =>
          resolved
      }
    }

    // ingredient to allergen 1 to 1 mapping
    val unsafeIngredientsMap = resolve(
      findAllergenMap(food),
      resolved = Map.empty
    )

    unsafeIngredientsMap.toSeq
      .sortBy(_._2) // allergen
      .map(_._1) // ingredient
      .mkString(",")
  }

  val sample: Seq[Food] = FileReader.read[Food]("sample_day21")
  assert(part1(sample) == 5)
  assert(part2(sample) == "mxmxvkd,sqjhc,fvjkl")

  val items: Seq[Food] = FileReader.read[Food]("input_day21")
  println(s"Part 1 answer:${part1(items)}")
  println(s"Part 2 answer:${part2(items)}")

}
