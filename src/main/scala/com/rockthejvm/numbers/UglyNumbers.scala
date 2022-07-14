package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

// Ugly numbers are those number whose prime factors are 2, 3 or 5 only
object UglyNumbers extends App {

  @tailrec
  def isUgly(n: Int): Boolean = {
    if (n == 1)
      true
    else if (n % 2 == 0)
      isUgly(n / 2)
    else if (n % 3 == 0)
      isUgly(n / 3)
    else if (n % 5 == 0)
      isUgly(n / 5)
    else
      false
  }

  println(isUgly(1)) // true
  println(isUgly(2)) // true
  println(isUgly(3)) // true
  println(isUgly(5)) // true
  println(isUgly(6)) // true
  println(isUgly(25)) // true
  println(isUgly(100)) // true
  println(isUgly(14)) // false
  println(isUgly(39)) // false

  def nthUglyApproach1(n: Int): Int = {
    @tailrec
    def nthUglyHelper(currentIndex: Int, currentNumber: Int): Int = {
      if (currentIndex == n && isUgly(currentNumber))
        currentNumber
      else if (!isUgly(currentNumber))
        nthUglyHelper(currentIndex, currentNumber + 1)
      else if (currentIndex != n)
        nthUglyHelper(currentIndex + 1, currentNumber + 1)
      else
        nthUglyHelper(currentIndex, currentNumber + 1)
    }

    nthUglyHelper(1, 1)
  }

  println(nthUglyApproach1(10)) // 12

  println(
    (1 to 20).map(nthUglyApproach1).toList
  ) // List(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36)

  def nthUglyApproach2(n: Int): Int = {
    @tailrec
    def nthUglyHelper(index: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int]): Int = {
      val min = min3(q2.head, q3.head, q5.head)

      if (index == n)
        min
      else {
        val newQ2 =
          (if (q2.head == min)
             q2.tail
           else
             q2).enqueue(min * 2)

        val newQ3 =
          (if (q3.head == min)
             q3.tail
           else
             q3).enqueue(min * 3)

        val newQ5 =
          (if (q5.head == min)
             q5.tail
           else
             q5).enqueue(min * 5)

        nthUglyHelper(index + 1, newQ2, newQ3, newQ5)
      }
    }

    def min3(a: Int, b: Int, c: Int): Int =
      if (a <= b && a <= c)
        a
      else if (b <= c)
        b
      else
        c

    if (n == 1)
      1
    else
      nthUglyHelper(2, Queue(2), Queue(3), Queue(5))
  }

  println(nthUglyApproach2(10)) // 12

  println(
    (1 to 20).map(nthUglyApproach2).toList
  ) // List(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36)

}
