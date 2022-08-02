package com.rockthejvm.strings

import scala.annotation.tailrec

object ReorganizeString extends App {

  def reorganizeString(str: String): String = {
    /*
          "aaabc"
          {a-3, b-1, c-1}
          reorganizeStringHelper({a-3, b-1, c-1}, \0 "") =
          reorganizeStringHelper({a-2, b-1, c-1}, 'a', "a") =
          reorganizeStringHelper({a-2, c-1}, 'b', "ab") =
          reorganizeStringHelper({a-1, c-1}, 'a', "aba") =
          reorganizeStringHelper({a-1}, 'c', "abac") =
          reorganizeStringHelper({}, 'a', "abaca") =
          "abaca"
     */
    @tailrec
    def reorganizeStringHelper(
        charCountMap: Map[Char, Int],
        forbiddenChar: Char = '\u0000',
        accumulator: String = ""
    ): String = {
      if (charCountMap.isEmpty)
        accumulator
      else {
        val newChar = charCountMap.filter(_._1 != forbiddenChar).maxBy(_._2)._1

        val newCharCountMap =
          if (charCountMap(newChar) == 1)
            charCountMap - newChar
          else
            charCountMap + (newChar -> (charCountMap(newChar) - 1))

        reorganizeStringHelper(newCharCountMap, newChar, accumulator + newChar)
      }
    }

    val charCountMap: Map[Char, Int] =
      str.foldLeft(Map[Char, Int]()) { case (map, newChar) =>
        map + (newChar -> (map.getOrElse(newChar, 0) + 1))
      }

    if (charCountMap.values.exists(_ > (str.length + 1) / 2))
      ""
    else
      reorganizeStringHelper(charCountMap)
  }

  println(reorganizeString("aaabc")) // abaca
  println(reorganizeString("aaab").isEmpty) // true
}
