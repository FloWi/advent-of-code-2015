package day02
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = input
      .map(Day02.parse)
      .map(Day02.calculateWrappingPaper)
      .sum

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

    val solution = input
      .map(Day02.parse)
      .map(Day02.calculateRibbon)
      .sum

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object Day02 {

  case class Prism(l: Int, w: Int, h: Int)

  def parse(line: String): Prism = {
    val List(l, w, h) = line.split("x").toList.map(_.toInt)
    Prism(l, w, h)
  }

  def calculateWrappingPaper(prism: Prism): Int = {
    val Prism(l, w, h) = prism
    val sides = List(
      l * w,
      w * h,
      h * l
    )
    val smallestSide = sides.min
    val total = sides.sum * 2 + smallestSide
    total
  }

  def calculateRibbon(prism: Prism): Int = {
    val Prism(l, w, h) = prism
    val sidePerimeters = List(
      2 * (l + w),
      2 * (w + h),
      2 * (h + l)
    )
    val smallestPerimeter = sidePerimeters.min
    val volume = l * w * h
    val total = smallestPerimeter + volume
    total
  }

}
