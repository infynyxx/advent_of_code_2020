package advent2020

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Day4 {
  def main(args: Array[String]) {
    val lines = Source
      .fromResource("input_day4.txt")
      .getLines()
      .toSeq
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

    val passports = lines.map(x => convertSingleBatchToMap(x.toSeq))
    //println(passports)

    val validPasswords = passports.filter(isValid2WithExtraValidation(_))
    println(validPasswords.size)
  }

  def convertSingleBatchToMap(buffer: Seq[String]) = {
    buffer
      .map(_.split(" "))
      .flatten
      .map(_.split(":"))
      .map(x => (x(0) -> x(1)))
      .toMap
  }

  def isValid(map: Map[String, String]): Boolean = {
    val requiredKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    requiredKeys.intersect(map.keySet).size == requiredKeys.size
  }

  def isValid2WithExtraValidation(map: Map[String, String]): Boolean = {
    val requiredKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    if (requiredKeys.intersect(map.keySet).size != requiredKeys.size) {
      return false
    }

    val birthYearString = map("byr")
    if (birthYearString.length() != 4) {
      return false
    }
    val birthYear = birthYearString.toInt
    if (!(birthYear >= 1920 && birthYear <= 2002)) {
      return false
    }

    val issueYearString = map("iyr")
    if (issueYearString.length() != 4) {
      return false
    }
    val issueYear = issueYearString.toInt
    if (!(issueYear >= 2010 && issueYear <= 2020)) {
      return false
    }

    val expirationYearString = map("eyr")
    if (expirationYearString.length() != 4) {
      return false
    }
    val expirationYear = expirationYearString.toInt
    if (!(expirationYear >= 2020 && expirationYear <= 2030)) {
      return false
    }

    val heightString = map("hgt")
    if (!(heightString.endsWith("cm") || heightString.endsWith("in"))) {
      return false
    }
    if (heightString.endsWith("cm")) {
      val height = heightString.replace("cm", "").toInt
      if (!(height >= 150 && height <= 193)) {
        return false
      }
    } else {
      val height = heightString.replace("in", "").toInt
      if (!(height >= 59 && height <= 76)) {
        return false
      }
    }

    val hairColorString = map("hcl")
    val hairColorRegex = """^#([0-9a-f]{6}$)""".r
    if (hairColorRegex.findFirstIn(hairColorString).isEmpty) {
      return false
    }

    val eyeColorString = map("ecl")
    val eyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    if (!eyeColors.contains(eyeColorString)) {
      return false
    }

    val passportId = map("pid")
    val regex = """^([0-9]{9}$)""".r
    if (regex.findFirstIn(passportId).isEmpty) {
      return false
    }

    true
  }

}
