package com.rockthejvm.strings

import scala.annotation.tailrec

object ReverseWords extends App {

  def reverseWords1(str: String): String = {
    @tailrec
    def reverseWordsHelper(remaining: Array[String], accumulator: String): String = {
      if (remaining.isEmpty)
        accumulator.trim
      else
        reverseWordsHelper(remaining.tail, remaining.head + " " + accumulator)
    }
    reverseWordsHelper(str.trim.split("\\s+"), "")
  }

  println(reverseWords1("Alice loves Scala")) // Scala loves Alice
  println(reverseWords1("       Hello         World     ")) // World Hello

  def reverseWords2(str: String): String = str.split(" ").filter(_.nonEmpty).reverse.mkString(" ")

  println(reverseWords2("Alice loves Scala")) // Scala loves Alice
  println(reverseWords2("       Hello         World     ")) // World Hello
}
