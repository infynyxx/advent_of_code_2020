package advent2020

import scala.io.Source

object Day5 {
  // front <- 0 to 63
  // back <- 64 to 127
  def main(args: Array[String]) {

    /** println(findSeatId("FBFBBFFRLR")) // 357
      *    println(findSeatId("BFFFBBFRRR")) // 567
      *    println(findSeatId("FFFBBBFRRR")) // 119
      *    println(findSeatId("BBFFBBFRLL")) // 820*
      */

    val seatIds = Source
      .fromResource("input_day5.txt")
      .getLines()
      .map(findSeatId(_))
      .toSeq

    val seatId =
      seatIds.sorted
        .sliding(2)
        .filter(x => x(1) - x(0) != 1)
        .toSeq
        .head
        .toSeq
        .head + 1

    println("the highest seat ID on a boarding pass " + seatIds.max) // 913
    println("seat id " + seatId) // 717
  }

  def binaryPartition(
      low: Int,
      high: Int,
      lowChar: Char,
      highChar: Char,
      char: Char
  ): (Int, Int) = {
    require(Set(lowChar, highChar).contains(char), "Invalid character " + char)
    val diff = high - low
    val lowerEnd = (diff / 2) + low
    val higherStart = lowerEnd + 1;
    if (char == lowChar) {
      (low, lowerEnd)
    } else {
      (higherStart, high)
    }
  }

  def findSeatId(boardingPass: String): Int = {
    val lowerSet = Set('F', 'L')
    val higherSet = Set('B', 'R')

    val charArray = boardingPass.toCharArray()

    val row = charArray
      .take(7)
      .foldLeft((0, 127)) { (initial, currentChar) =>
        {
          binaryPartition(initial._1, initial._2, 'F', 'B', currentChar)
        }
      }
      ._1

    val column = charArray
      .takeRight(3)
      .foldLeft((0, 7)) { (initial, currentChar) =>
        binaryPartition(initial._1, initial._2, 'L', 'R', currentChar)
      }
      ._1

    (row * 8) + column
  }
}
