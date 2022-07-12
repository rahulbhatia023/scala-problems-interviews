package com.rockthejvm.numbers

import scala.annotation.tailrec

object ParseInteger extends App {

  @tailrec
  def parseInteger(string: String): Int = {
    val WHITESPACE = ' '
    val PLUS = '+'
    val MINUS = '-'
    val DIGITS = "0123456789".toSet

    def integerRangeEnd(sign: Int): Int =
      if (sign >= 0)
        Int.MaxValue
      else
        Int.MinValue

    @tailrec
    def parseIntegerHelper(remaining: String, sign: Int, accumulator: Int): Int = {
      if (remaining.isEmpty || !DIGITS.contains(remaining.charAt(0)))
        accumulator
      else {
        val newDigit = remaining.charAt(0) - '0'
        val tentativeResult = accumulator * 10 + newDigit * sign

        if ((sign >= 0) != (tentativeResult >= 0))
          integerRangeEnd(sign)
        else
          parseIntegerHelper(remaining.substring(1), sign, tentativeResult)
      }
    }

    if (string.isEmpty)
      0
    else if (string.charAt(0) == WHITESPACE)
      parseInteger(string.substring(1))
    else if (string.charAt(0) == PLUS)
      parseIntegerHelper(string.substring(1), 1, 0)
    else if (string.charAt(0) == MINUS)
      parseIntegerHelper(string.substring(1), -1, 0)
    else
      parseIntegerHelper(string, 1, 0)
  }

  println(parseInteger("")) // 0
  println(parseInteger("String")) // 0
  println(parseInteger("1")) // 1
  println(parseInteger("-1")) // -1
  println(parseInteger("   Scala")) // 0
  println(parseInteger("   4256")) // 4256
  println(parseInteger("   -4256")) // -4256
  println(parseInteger("   +4256")) // 4256
  println(parseInteger("42 is the meaning of life")) // 42
  println(parseInteger("  42 is the meaning of life")) // 42
  println(parseInteger(Int.MaxValue.toString)) // 2147483647
  println(parseInteger(Int.MinValue.toString)) // -2147483648
  println(parseInteger("357893276583265783265783")) // 2147483647
  println(parseInteger("-357893276583265783265783")) // -2147483648
}
