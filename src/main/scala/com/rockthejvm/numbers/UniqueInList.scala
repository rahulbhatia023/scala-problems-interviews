package com.rockthejvm.numbers

import scala.annotation.tailrec

/**
 * If a list contains multiple elements which are repeated but contains only one unique element, then find the unique
 * element
 */
object UniqueInList extends App {

  def unique(list: List[Int]): Int = {
    /*
      Complexity: O(N^2) Time, O(1) Space
     */
    @tailrec
    def approach1(remaining: List[Int]): Int = {
      if (remaining.isEmpty)
        throw new IllegalArgumentException("List is Empty")
      else {
        val head = remaining.head
        val headCount = list.count(_ == head)
        if (headCount == 1)
          head
        else
          approach1(remaining.tail)
      }
    }

    /*
      Complexity: O(N) Time, O(N) Space
     */
    @tailrec
    def approach2(remaining: List[Int], occurrences: Map[Int, Int] = Map()): Int = {
      if (remaining.isEmpty)
        occurrences.filter(_._2 == 1).head._1
      else {
        val element = remaining.head
        val elementOccurrence = occurrences.getOrElse(element, 0)
        approach2(remaining.tail, occurrences + (element -> (elementOccurrence + 1)))
      }
    }

    /*
      Complexity: O(N) Time, O(N) Space with some optimization
     */
    @tailrec
    def approach3(remaining: List[Int], memory: Set[Int] = Set()): Int = {
      if (remaining.isEmpty)
        memory.head
      else if (memory.contains(remaining.head))
        approach3(remaining.tail, memory - remaining.head)
      else
        approach3(remaining.tail, memory + remaining.head)
    }

    /*
      This approach works only if numbers are repeated not more than twice
      Complexity: O(N) Time, O(1) Space
     */
    def approach4(list: List[Int]): Int = list.foldLeft(0)(_ ^ _)

    // approach1(list)
    // approach2(list)
    approach3(list)
    // approach4(list)
  }

  println(unique(List(1)))
  println(unique(List(1, 2, 1)))
  println(unique(List(1, 2, 1, 2, 3, 4, 4, 5, 5, 5)))

  val elements = (1 to 100000).toList
  println(unique(elements ++ List(2486) ++ elements))
}
