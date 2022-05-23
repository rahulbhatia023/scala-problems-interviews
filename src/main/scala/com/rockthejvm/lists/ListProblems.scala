package com.rockthejvm.lists

import scala.annotation.{tailrec, targetName}

abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  // This notation of having colon(:) at the end of method name makes it right associative
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()

  override def tail: RList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()

  override def length: Int = 0

  override def reverse: RList[Nothing] = this
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringHelper(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringHelper(remaining.tail, s"$result${remaining.head}, ")
    }

    s"[${toStringHelper(this, "")}]"
  }

  // Complexity: O(min(N, index)
  override def apply(index: Int): T = {
    @tailrec
    def applyHelper(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyHelper(remaining.tail, currentIndex + 1)
    }

    if (index < 0) throw new NoSuchElementException()
    else applyHelper(this, 0)
  }

  // Complexity: O(N)
  override def length: Int = {
    @tailrec
    def lengthHelper(list: RList[T], accumulator: Int): Int = {
      if (list.isEmpty) accumulator
      else lengthHelper(list.tail, accumulator + 1)
    }

    lengthHelper(this, 0)
  }

  // Complexity: O(N)
  override def reverse: RList[T] = {
    @tailrec
    def reverseListHelper(list: RList[T], accumulator: RList[T]): RList[T] = {
      if (list.isEmpty) accumulator
      else reverseListHelper(list.tail, list.head :: accumulator)
    }

    reverseListHelper(this, RNil)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def fromHelper(it: Iterable[T], accumulator: RList[T]): RList[T] = {
      if (it.isEmpty) accumulator
      else fromHelper(it.tail, it.head :: accumulator)
    }

    fromHelper(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  //val list = RNil.::(3).::(2).::(1)
  val list = 1 :: 2 :: 3 :: 9 :: 8 :: 7 :: RNil

  println(list)
  // [1, 2, 3, 9, 8, 7]

  println(list(0)) // 1
  println(list(1)) // 2
  println(list(2)) // 3

  //println(list(-1))
  // NoSuchElementException

  // println(list(4))
  // NoSuchElementException

  println(list.length) // 6
  println(list.reverse) // [7, 8, 9, 3, 2, 1]

  val bigList = RList.from(1 to 10000)
  println(bigList.length) // 10000
}
