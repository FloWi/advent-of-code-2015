package day20
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val solution = findLowestHouseNumber(36000000)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def findLowestHouseNumber(minAmountOfPresents: Int): Int = {
    val houses = collection.mutable.HashMap.empty[Int, Int]
    1.to(minAmountOfPresents / 10)
      .foreach(i =>
        i.to(minAmountOfPresents / 10, i)
          .foreach { j =>
            houses.updateWith(j) {
              case None    => Some(i * 10)
              case Some(h) => Some(h + i * 10)
            }
          }
      )

    houses.find(_._2 >= minAmountOfPresents).get._1

  }
}

object part2 {

  def main(args: Array[String]) = {

    val solution = findLowestHouseNumber(36000000)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def findLowestHouseNumber(minAmountOfPresents: Int): Int = {
    val houses = collection.mutable.HashMap.empty[Int, Int]
    1.to(minAmountOfPresents / 10)
      .foreach(i =>
        i.to(minAmountOfPresents / 10, i)
          .take(50)
          .foreach { j =>
            houses.updateWith(j) {
              case None    => Some(i * 11)
              case Some(h) => Some(h + i * 11)
            }
          }
      )

    houses.find(_._2 >= minAmountOfPresents).get._1

  }
}

object Day20 {}
