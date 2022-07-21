package com.rockthejvm.strings

import scala.annotation.tailrec

object StringProblems extends App {

  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharactersHelper(remaining: String, map: Map[Char, Int]): Map[Char, Int] = {
      if (remaining.isEmpty)
        map
      else
        countCharactersHelper(remaining.tail, map + (remaining.head -> (map.getOrElse(remaining.head, 0) + 1)))
    }

    countCharactersHelper(s, Map())
  }

  println(countCharacters("Scala")) // Map(S -> 1, c -> 1, a -> 2, l -> 1)

  def checkAnagrams1(stringA: String, stringB: String): Boolean = countCharacters(stringA) == countCharacters(stringB)

  def checkAnagrams2(stringA: String, stringB: String): Boolean = stringA.sorted == stringB.sorted

  println(checkAnagrams1("desserts", "stressed")) // true
  println(checkAnagrams1("scala", "haskell")) // false

  println(checkAnagrams2("desserts", "stressed")) // true
  println(checkAnagrams2("scala", "haskell")) // false
}
