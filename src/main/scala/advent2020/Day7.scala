package advent2020

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// https://adventofcode.com/2020/day/7
object Day7 {

  case class Node(source: String, destination: String)

  class Graph(val map: Map[String, Node]) {}

  def buildGraph(lines: Vector[String]) = {
    val map = collection.mutable.Map[String, ArrayBuffer[Node]]()
    val nodes = lines
      .map(parseLine(_))
      .map(x => x._2.map(_ -> x._1))
      .flatten
      .map(x => Node(x._1, x._2))

    nodes.foreach { case (node: Node) =>
      val curVal = map.getOrElse(node.source, ArrayBuffer())
      curVal.append(node)
      map.update(node.source, curVal)
    }
    map.map(x => x._1 -> x._2.toVector).toMap
  }

  def main(args: Array[String]) {
    val lines =
      Source.fromResource("input_day7.txt").getLines().toVector

    val map = lines.map(parseLine(_)).toMap

    //println(map)

    assert(map.size == lines.size)
    //val graph = buildGraph(lines)

    val needle = "shiny gold"
    //val items = search(map, needle, mutable.Set.empty[String])

    val bags = search(map, needle, mutable.Set.empty[String])

    println(bags)
    println(bags.size) // 281 too low
    println(s"map size = ${map.size}")
  }

  def search2(
      map: Map[String, Set[String]],
      needle: String,
      visited: mutable.Set[String]
  ) = {
    map
      .map { case (key, value) =>
        if (visited.contains(key)) {
          None
        } else {
          visited.add(needle)
          if (value.contains(needle)) {
            Some(Seq(key))
          } else {
            Some(
              value
                .filter(!visited.contains(_))
                .filter(x => {
                  map(x).contains(needle)
                })
                .toSeq ++ value
                .filter(!visited.contains(_))
                .filter(x => {
                  !map(x).contains(needle)
                })
                .map(search(map, _, visited))
                .flatten
            )
          }
        }
      }
      .filter(_.isDefined)
      .flatten
      .flatten
      .toSeq
  }

  def search(
      map: Map[String, Set[String]],
      needle: String,
      visited: mutable.Set[String]
  ): Seq[String] = {
    map
      .map { case (key, value) =>
        if (visited.contains(key)) {
          None
        } else {
          visited.add(needle)
          if (value.contains(needle)) {
            Some(Seq(key))
          } else {
            Some(
              value
                .filter(!visited.contains(_))
                .map(search(map, _, visited))
                .flatten
            )
          }
        }
      }
      .filter(_.isDefined)
      .flatten
      .flatten
      .toSeq
  }

  def parseLine(line: String) = {
    val splits = line.replace(".", "").split("contain").map(_.trim())
    val value = if (splits(1).contains("no other bags")) {
      Set.empty[String]
    } else {
      splits(1)
        .split(",")
        .map(
          _.trim()
            .split(" ")
            .map(_.trim())
            .drop(1)
            .dropRight(1) // drop bag / bags word at the end
            .mkString(" ")
        )
        .toSet
    }
    splits(0).split(" ").dropRight(1).mkString(" ") -> value
  }
}
