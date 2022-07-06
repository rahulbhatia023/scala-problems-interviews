package com.rockthejvm.numbers

import scala.util.Random

object ApproximatePi extends App {

  def approximatePi(nPoints: Int): Double = {
    val random = new Random(System.currentTimeMillis())

    val nPointsInsideCircle = (1 to nPoints)
      .map { _ =>
        val x = random.nextDouble()
        val y = random.nextDouble()

        (x * x) + (y * y)
      }
      .count(_ < 1)

    nPointsInsideCircle * 4.0 / nPoints
  }

  println(s"Reference: ${Math.PI}")

  println(approximatePi(1000)) // 3.192
  println(approximatePi(10000)) // 3.1396
  println(approximatePi(100000)) // 3.14344
  println(approximatePi(1000000)) // 3.141968
  println(approximatePi(10000000)) // 3.14117
  println(approximatePi(100000000)) // 3.141204
}
