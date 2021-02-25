package advent2020

import scala.collection.mutable.ArrayBuffer

object Day8 {
  def main(args: Array[String]) {
    val lines = inputToVector("input_day8.txt");
    println(
      pixelate(lines.head)
    )
  }

  def pixelate(input: String) = {
    val partitioned = input
      .toCharArray()
      .grouped(25 * 6)
      .map(_.toVector)
      .toVector

    val x = partitioned.map(elem => elem.filter(char => char != '0').size)
      .zipWithIndex
      .toArray.sortWith(_._1 > _._1)
      .toVector
    val layerIndex = x.head._2
    val layer = partitioned(layerIndex)
    val oneAndTwoDigits = layer.foldLeft((0, 0)) {
      (a, char) =>
        if (char == '1') {
          (a._1 + 1, a._2)
        } else if (char == '2') {
          (a._1, a._2 + 1)
        } else {
          (a._1, a._2)
        }
    }
    println(layer)
    println(oneAndTwoDigits)
    println(oneAndTwoDigits._1 * oneAndTwoDigits._2)
  }

  def pixelatePart2(input: String) = {
  }
}