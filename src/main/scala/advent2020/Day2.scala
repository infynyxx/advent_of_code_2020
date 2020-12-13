package advent2020

import scala.io.Source;

/** --- Day 2: Password Philosophy ---
  *
  * Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.
  *
  * The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.
  *
  * Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.
  *
  * To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.
  *
  * For example, suppose you have the following list:
  *
  * 1-3 a: abcde
  * 1-3 b: cdefg
  * 2-9 c: ccccccccc
  *
  * Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
  *
  * In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.
  */

object Day2 {
  def main(args: Array[String]) {
    val passwords = Source
      .fromResource("input_day2.txt")
      .getLines()
      .map(parsePasswordPolicy(_))

    val validPasswords = passwords.filter(isValidPart2(_)).toSeq
    println(validPasswords.size)
  }

  case class PasswordPolicy(occurance: (Int, Int), char: Char, password: String)

  /**  @param input 9-13 x: xxxxxxxxxxxxxxxx
    */
  def parsePasswordPolicy(input: String) = {
    val splits = input.trim().split(" ")
    if (splits.length != 3) {
      throw new Exception("Invalid input: " + input);
    }
    val occurance = splits(0).split("-").toSeq match {
      case Seq(low, high) => (low.toInt, high.toInt)
      case _              => throw new Exception("Invalid input " + input);
    }

    val char = splits(1).toCharArray().head

    val password = splits(2)
    PasswordPolicy(occurance, char, password)
  }

  def isValid(passwordPolicy: PasswordPolicy): Boolean = {
    val count = passwordPolicy.password.filter(_ == passwordPolicy.char).size
    count >= passwordPolicy.occurance._1 && count <= passwordPolicy.occurance._2
  }

  def isValidPart2(passwordPolicy: PasswordPolicy): Boolean = {
    val occurance = passwordPolicy.occurance
    val password = passwordPolicy.password
    if (
      !(occurance._1 <= password.length() && occurance._2 <= password.length())
    ) {
      throw new Exception("Invalid input" + passwordPolicy);
    }
    // exactly once occurance
    Seq(
      password.charAt(occurance._1 - 1) == passwordPolicy.char,
      password.charAt(occurance._2 - 1) == passwordPolicy.char
    ).filter(_ == true).size == 1
  }
}
