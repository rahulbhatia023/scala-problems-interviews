package com.rockthejvm.strings

import scala.annotation.tailrec

object CompareVersionNumbers extends App {

  def compareVersionNumbers(version1: String, version2: String): Int = {
    @tailrec
    def compareVersionNumbersHelper(version1Remaining: Array[String], version2Remaining: Array[String]): Int = {
      if (version1Remaining.isEmpty && version2Remaining.isEmpty)
        0
      else if (version1Remaining.isEmpty) {
        /*
          1.0 = 1.0.0
          1.0 < 1.0.1
         */
        if (version2Remaining.exists(_ != "0"))
          -1
        else
          0
      } else if (version2Remaining.isEmpty) {
        /*
          1.0.0 = 1.0
          1.0.1 > 1.0
         */
        if (version1Remaining.exists(_ != "0"))
          1
        else
          0
      } else {
        /*
          Both versions are non empty
         */
        if (version1Remaining.head < version2Remaining.head)
          -1
        else if (version1Remaining.head > version2Remaining.head)
          1
        else
          compareVersionNumbersHelper(version1Remaining.tail, version2Remaining.tail)
      }
    }

    compareVersionNumbersHelper(version1.split("\\."), version2.split("\\."))
  }

  println(compareVersionNumbers("0.9", "1.0.3.4")) // -1
  println(compareVersionNumbers("1.0.3.4", "1.1.0")) // -1
  println(compareVersionNumbers("1.1.0", "2.0")) // -1
  println(compareVersionNumbers("2.0", "2.1")) // -1
  println(compareVersionNumbers("2.0", "2.01")) // -1
  println(compareVersionNumbers("1.1.0", "1.1.0")) // 0
  println(compareVersionNumbers("1.1.0", "1.1.0.1")) // -1
  println(compareVersionNumbers("1.1.0.1", "1.1.0")) // 1
  println(compareVersionNumbers("1.0", "1.0.0")) // 0
  println(compareVersionNumbers("1.0.0", "1.0")) // 0
}
