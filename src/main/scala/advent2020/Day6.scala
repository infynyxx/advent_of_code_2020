package advent2020

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

// https://adventofcode.com/2020/day/6
object Day6 {
  def main(args: Array[String]) {
    val lines = Source
      .fromResource("input_day6.txt")
      .getLines()
      .foldLeft(ArrayBuffer.empty[ArrayBuffer[String]]) { (list, line) =>
        if (!line.trim().isEmpty()) {
          if (list.isEmpty) {
            ArrayBuffer(ArrayBuffer(line))
          } else {
            list.last += line
            list
          }
        } else {
          list += ArrayBuffer()
          list
        }
      }

    val part1 = lines
      .map(x => x.flatMap(_.toCharArray()).toSet.size)
      .sum
    println(part1) // 6291

    val part2 = lines
      .map(group => group.map(_.toCharArray().toSet).reduce(_.intersect(_)))
      .map(_.size)
      .sum
    println(part2) // 3052
  }
}
