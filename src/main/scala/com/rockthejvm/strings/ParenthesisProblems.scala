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
}
