package day21

import day21.Day21.ShoppingItem
import day21.part1.shopContentInput
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day21Part1Test extends AnyFunSuite with Matchers {

  val bossInput = """
    Hit Points: 109
    Damage: 8
    Armor: 2
    """.trim

  test("example 1 - parsing") {

    val actual = Day21.parseShoppingList(shopContentInput)
    val actualBossStats = Day21.parse(bossInput)

    actual.weapons should contain theSameElementsAs List(
      ShoppingItem("Dagger", 8, 4, 0),
      ShoppingItem("Shortsword", 10, 5, 0),
      ShoppingItem("Warhammer", 25, 6, 0),
      ShoppingItem("Longsword", 40, 7, 0),
      ShoppingItem("Greataxe", 74, 8, 0)
    )

    actual.armors should contain theSameElementsAs List(
      ShoppingItem("Leather", 13, 0, 1),
      ShoppingItem("Chainmail", 31, 0, 2),
      ShoppingItem("Splintmail", 53, 0, 3),
      ShoppingItem("Bandedmail", 75, 0, 4),
      ShoppingItem("Platemail", 102, 0, 5)
    )

    actual.rings should contain theSameElementsAs List(
      ShoppingItem("Damage +1", 25, 1, 0),
      ShoppingItem("Damage +2", 50, 2, 0),
      ShoppingItem("Damage +3", 100, 3, 0),
      ShoppingItem("Defense +1", 20, 0, 1),
      ShoppingItem("Defense +2", 40, 0, 2),
      ShoppingItem("Defense +3", 80, 0, 3)
    )

    actualBossStats.hitPoints shouldBe 109
    actualBossStats.damage shouldBe 8
    actualBossStats.armor shouldBe 2
  }

}

class Day21Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
