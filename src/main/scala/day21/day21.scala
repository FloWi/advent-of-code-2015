package day21
import helper.Helper._

object part1 {

  val shopContentInput = """
Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3
  """.trim

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = ???

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

    val solution = ???

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object Day21 {
  case class ShoppingList(weapons: List[ShoppingItem], armors: List[ShoppingItem], rings: List[ShoppingItem])
  def parseShoppingList(input: String): ShoppingList = {

    val List(weaponsInput, armorInput, ringsInput) = input.split("\n\n").toList.map(_.linesIterator.toList.tail)
    //Weapons:    Cost  Damage  Armor
    //Dagger        8     4       0
    //Damage +1    25     1       0

    val weapons = weaponsInput.map(ShoppingItem.parse)
    val armor = armorInput.map(ShoppingItem.parse)
    val rings = ringsInput.map(ShoppingItem.parse)

    ShoppingList(weapons, armor, rings)
  }

  def parse(bossInput: String): BossStats = {
    /*
      Hit Points: 109
      Damage: 8
      Armor: 2
     */

    val List(hp, damage, armor) = bossInput.linesIterator.toList.map(_.split(": ")(1).toInt)
    BossStats(hp, damage, armor)
  }

  case class ShoppingItem(name: String, cost: Int, damage: Int, armor: Int)

  object ShoppingItem {
    def parse(line: String): ShoppingItem = {
      val lineItemRegex = "\\s{2,}".r

      val name :: properties = lineItemRegex.split(line).toList
      val List(cost, damage, armor) = properties.map(_.toInt)
      ShoppingItem(name, cost, damage, armor)
    }
  }

  case class BossStats(hitPoints: Int, damage: Int, armor: Int)

}
