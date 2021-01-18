package day15
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input, Day15.combinations)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(input: List[String], combinations: Seq[Vector[Int]]) = {

    val ingredientList = Day15.parseIngredients(input)

    val flipped = Day15
      .flipList(ingredientList)

    val propertiesForScoreCalculation = flipped.filterNot(_._1 == "calories")

    val all = combinations.map { factors =>
      factors -> Day15.calculateScoreForCombination(propertiesForScoreCalculation, factors)
    }

    val result = all.maxBy(_._2)

    result._2
  }

}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input, Day15.combinations)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(input: List[String], combinations: Seq[Vector[Int]]) = {

    val ingredientList = Day15.parseIngredients(input)
    val flipped = Day15
      .flipList(ingredientList)

    val propertiesForScoreCalculation = flipped.filterNot(_._1 == "calories")

    val calorieProp = flipped.filter(_._1 == "calories")

    val combinationsWith500kcal = combinations.filter { factors =>
      Day15.calculateScoreForCombination(calorieProp, factors) == 500
    }

    val all = combinationsWith500kcal.map { factors =>
      factors -> Day15.calculateScoreForCombination(propertiesForScoreCalculation, factors)
    }

    val result = all.maxBy(_._2)

    result._2
  }

}

object Day15 {
  def parseIngredients(input: List[String]): List[(String, List[(String, Int)])] = {
    input.map { line =>
      val List(name, propertiesString) = line.split(": ").toList
      val properties = propertiesString
        .split(", ")
        .toList
        .map { propString =>
          val List(prop, value) = propString.split(" ").toList
          prop -> value.toInt
        }

      name -> properties

    }
  }

  def combinations: Seq[Vector[Int]] = {
    for {
      i <- 1.to(100)
      j <- 1.to(100 - i)
      k <- 1.to(100 - i - j)
      l <- 1.to(100 - i - j - k)
      if i + j + k + l == 100
    } yield Vector(i, j, k, l)
  }

  def flipList(ingredients: List[(String, List[(String, Int)])]): List[(String, List[(String, Int)])] = {
    // original
    // Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
    // Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

    //flipped:
    // capacity: Butterscotch -1, Cinnamon 2
    // durability: Butterscotch -2, Cinnamon 3
    // flavor: Butterscotch 6, Cinnamon -2
    // texture: Butterscotch 3, Cinnamon -1
    // calories: Butterscotch 8, Cinnamon 3

    ingredients
      .flatMap { case (name, props) => props.map { case (prop, value) => (prop, name, value) } }
      .groupBy(_._1)
      .mapValues(list => list.map(tup => (tup._2, tup._3)))
      .toList
  }

  def calculateScoreForCombination(propertyIngredientList: List[(String, List[(String, Int)])], scaleFactors: Vector[Int]) = {

    val scores = propertyIngredientList.map { case tup @ (prop, ingredients) =>
      val values = ingredients.zip(scaleFactors).map { case ((_, value), factor) => value * factor }
      values.sum
    }

    if (scores.exists(_ < 0)) 0
    else scores.product

  }
}
