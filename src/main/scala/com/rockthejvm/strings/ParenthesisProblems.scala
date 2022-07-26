package com.rockthejvm.strings

import scala.annotation.tailrec

object ParenthesisProblems extends App {

  def hasValidParenthesis(str: String): Boolean = {
    @tailrec
    def hasValidParenthesisHelper(remaining: String, openParenthesisCount: Int): Boolean = {
      if (remaining.isEmpty)
        openParenthesisCount == 0
      else if (remaining.head == ')' && openParenthesisCount == 0)
        false
      else if (remaining.head == ')')
        hasValidParenthesisHelper(remaining.tail, openParenthesisCount - 1)
      else
        hasValidParenthesisHelper(remaining.tail, openParenthesisCount + 1)
    }

    hasValidParenthesisHelper(str, 0)
  }

  println(hasValidParenthesis("()")) // true
  println(hasValidParenthesis("()()")) // true
  println(hasValidParenthesis("(())")) // true
  println(hasValidParenthesis(")(")) // false

  def generateAllValidParenthesis(n: Int): List[String] = {
    @tailrec
    def generateAllValidParenthesisHelper(nRemaining: Int, currentStrings: Set[String]): Set[String] = {
      if (nRemaining == 0)
        currentStrings
      else {
        val newStrings =
          for {
            string <- currentStrings
            index <- 0 until string.length
          } yield {
            val (before, after) = string.splitAt(index)
            s"$before()$after"
          }

        generateAllValidParenthesisHelper(nRemaining - 1, newStrings)
      }
    }

    assert(n >= 0)

    if (n == 0)
      List.empty
    else
      generateAllValidParenthesisHelper(n - 1, Set("()")).toList
  }

  println(generateAllValidParenthesis(1)) // List(())
  println(generateAllValidParenthesis(2)) // List(()(), (()))
  println(generateAllValidParenthesis(3)) // List((()()), ((())), ()()(), (())(), ()(()))
}
