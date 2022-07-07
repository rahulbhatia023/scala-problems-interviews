package com.rockthejvm.numbers

/*
    Given a list of non-negative integers, arrange them such that they form the largest number.
    The result might be huge so return a string.

    List(10, 2) => "210"
    List(3, 30, 5, 9, 34) => "9534330"
 */
object LargestNumber extends App {

  def largestNumber(numbers: List[Int]): String = {
    implicit val ordering: Ordering[Int] = Ordering.fromLessThan((a, b) =>
      /*
          concatenate a with b => ab
          concatenate b with a => ba
          comparison: a comes before b if ab >= ba
       */

      (a.toString + b.toString).compareTo(b.toString + a.toString) >= 0
    )

    if (numbers.isEmpty || numbers.head == 0)
      "0"
    else
      numbers.sorted.mkString("")
  }

  println(largestNumber(List(10, 2))) // 210
  println(largestNumber(List(3, 30, 5, 9, 34))) // 9534330
  println(largestNumber(List(2020, 20, 1010, 10, 2, 22))) // 222202020101010
  println(largestNumber(List(1))) // 1
  println(largestNumber(List())) // 0
  println(largestNumber(List(0, 0, 0))) // 0
}
