package day16
import Day16._
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val giftWrapProps = Day16.parseGiftWrapProps(giftWrapPropsInput.trim.split("\n").toList)

    val isValid: (String, Int, Map[String, Int]) => Boolean = (k, v, map) => map(k) == v
    val solution = Day16
      .solve(
        input,
        isValid(_, _, giftWrapProps)
      )
      .id

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val giftWrapProps = Day16.parseGiftWrapProps(giftWrapPropsInput.trim.split("\n").toList)

    val solution = Day16
      .solve(
        input,
        isValid(_, _, giftWrapProps)
      )
      .id

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def isValid(propertyName: String, value: Int, giftWrapProperties: Map[String, Int]): Boolean = {

    propertyName match {
      case "cats" | "trees"         => value > giftWrapProperties(propertyName)
      case "pomerians" | "goldfish" => value < giftWrapProperties(propertyName)
      case _                        => value == giftWrapProperties(propertyName)
    }
  }

}

object Day16 {
  case class Aunt(id: Int, facts: Map[String, Int])

  val giftWrapPropsInput = """
children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
  """

  def parse(line: String): Aunt = {

    // Sue 500: perfumes: 4, cars: 9, trees: 4
    val (name, factsString) = line.splitAt(line.indexOf(": "))

    val facts = factsString
      .drop(2)
      .split(", ")
      .map { factString =>
        val List(name, value) = factString.split(": ").toList
        name -> value.toInt
      }
      .toMap

    Aunt(name.replace("Sue ", "").toInt, facts)
  }

  def parseGiftWrapProps(input: List[String]): Map[String, Int] = {
    input.map { line =>
      val List(name, value) = line.split(": ").toList
      name -> value.toInt
    }.toMap
  }

  def solve(input: List[String], isValid: (String, Int) => Boolean): Aunt = {

    val aunts = input
      .map(Day16.parse)

    aunts
      .find(aunt => {
        aunt.facts.forall { case (k, v) => isValid(k, v) }
      })
      .get
  }
}
