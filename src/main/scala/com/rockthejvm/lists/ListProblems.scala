package com.rockthejvm.lists

import scala.annotation.{tailrec, targetName}

abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  // This notation of having colon(:) at the end of method name makes it right associative
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()

  override def tail: RList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()
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

  override def apply(index: Int): T = {
    @tailrec
    def applyHelper(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyHelper(remaining.tail, currentIndex + 1)
    }

    if (index < 0) throw new NoSuchElementException()
    else applyHelper(this, 0)
  }
}

object ListProblems extends App {
  //val list = RNil.::(3).::(2).::(1)
  val list = 1 :: 2 :: 3 :: RNil

  println(list)
  println(list(0)) // 1
  println(list(1)) // 2
  println(list(2)) // 3

  //println(list(-1))
  // NoSuchElementException

  // println(list(4))
  // NoSuchElementException
}
