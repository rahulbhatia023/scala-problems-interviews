package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumberOps {

  implicit class RichInt(n: Int) {

    /*
            isPrime(11) = isPrimeHelper(2)
              Math.sqrt(Math.abs(11)) = 3.3
            = if(2 > 3.3) -> false
            = if(11 % 2 == 0) -> false
            = isPrimeHelper(3)
            = if(3 > 3.3) -> false
            = if(11 % 3 == 0) -> false
            = isPrimeHelper(4)
            = if(4 > 3.3) -> true
            = true

            isPrime(15) = isPrimeHelper(2)
              Math.sqrt(Math.abs(11)) = 3.8
            = if(2 > 3.8) -> false
            = if(15 % 2 == 0) -> false
            = isPrimeHelper(3)
            = if(3 > 3.8) -> false
            = if(15 % 3 == 0) -> true
            = false

            Complexity: O(sqrt(N))
     */
    def isPrime: Boolean = {
      @tailrec
      def isPrimeHelper(currentDivisor: Int): Boolean = {
        if (currentDivisor > Math.sqrt(Math.abs(n)))
          true
        else if (n % currentDivisor == 0)
          false
        else
          isPrimeHelper(currentDivisor + 1)
      }

      if (n == -1 || n == 0 || n == 1)
        false
      else
        isPrimeHelper(2)
    }

  }

}

object NumberProblems extends App {
  import NumberOps.*

  println(2.isPrime) // true
  println(15.isPrime) // false
  println(2003.isPrime) // true
  println(2731189.isPrime) // true
  println(517935871.isPrime) // false
  println(1.isPrime) // false
  println(0.isPrime) // false
  println(-2003.isPrime) // true
}
