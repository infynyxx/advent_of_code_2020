package advent2020

import scala.io.Source

object Day1 {
  def find2Entries(expenses: Seq[Int]) = {
    val nested =
      (for (i <- expenses) yield for (j <- expenses) yield (i, j)).flatten
    nested
      .filter(predicate => predicate._1 + predicate._2 == 2020)
      .map(x => x._1 * x._2)
      .head
  }

  def main(args: Array[String]) {
    val expenses = Source
      .fromResource("input_day1.txt")
      .getLines()
      .map(x => Integer.parseInt(x))
      .toSeq
    println(find2Entries(expenses))
  }
}
