package com.rockthejvm.lists

import scala.annotation.{tailrec, targetName}
import scala.jdk.Accumulator
import scala.util.Random

abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  // This notation of having colon(:) at the end of method name makes it right associative
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]

  def runLengthEncoding: RList[(T, Int)]

  def duplicateEach(k: Int): RList[T]

  def rotate(k: Int): RList[T]

  def sample(k: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()

  override def tail: RList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  override def runLengthEncoding: RList[(Nothing, Int)] = RNil

  override def duplicateEach(k: Int): RList[Nothing] = RNil

  override def rotate(k: Int): RList[Nothing] = RNil

  override def sample(k: Int): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringHelper(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty)
        result
      else if (remaining.tail.isEmpty)
        s"$result${remaining.head}"
      else
        toStringHelper(remaining.tail, s"$result${remaining.head}, ")
    }

    s"[${toStringHelper(this, "")}]"
  }

  // Complexity: O(min(N, index)
  override def apply(index: Int): T = {
    @tailrec
    def applyHelper(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index)
        remaining.head
      else
        applyHelper(remaining.tail, currentIndex + 1)
    }

    if (index < 0)
      throw new NoSuchElementException()
    else
      applyHelper(this, 0)
  }

  // Complexity: O(N)
  override def length: Int = {
    @tailrec
    def lengthHelper(list: RList[T], accumulator: Int): Int = {
      if (list.isEmpty)
        accumulator
      else
        lengthHelper(list.tail, accumulator + 1)
    }

    lengthHelper(this, 0)
  }

  // Complexity: O(N)
  override def reverse: RList[T] = {
    @tailrec
    def reverseListHelper(list: RList[T], accumulator: RList[T]): RList[T] = {
      if (list.isEmpty)
        accumulator
      else
        reverseListHelper(list.tail, list.head :: accumulator)
    }

    reverseListHelper(this, RNil)
  }

  // Concatenation
  // Complexity: O(M + N), where M and N are the length of respective lists
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatHelper(remainingList: RList[S], accumulator: RList[S]): RList[S] = {
      if (remainingList.isEmpty)
        accumulator
      else
        concatHelper(remainingList.tail, remainingList.head :: accumulator)
    }

    concatHelper(this.reverse, anotherList)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtHelper(currentIndex: Int, left: RList[T], right: RList[T]): RList[T] = {
      if (currentIndex == index)
        left.reverse ++ right.tail
      else
        removeAtHelper(currentIndex + 1, right.head :: left, right.tail)
    }

    if (index < 0)
      this
    else
      removeAtHelper(0, RNil, this)
  }

  // Complexity: O(N)
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapHelper(remainingList: RList[T], accumulator: RList[S]): RList[S] = {
      if (remainingList.isEmpty)
        accumulator.reverse
      else
        mapHelper(remainingList.tail, f(remainingList.head) :: accumulator)
    }

    mapHelper(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapHelper(remainingList: RList[T], accumulator: RList[S]): RList[S] = {
      if (remainingList.isEmpty)
        accumulator
      else
        flatMapHelper(remainingList.tail, accumulator ++ f(remainingList.head))
    }

    flatMapHelper(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterHelper(remainingList: RList[T], accumulator: RList[T]): RList[T] = {
      if (remainingList.isEmpty)
        accumulator.reverse
      else if (f(remainingList.head))
        filterHelper(remainingList.tail, remainingList.head :: accumulator)
      else
        filterHelper(remainingList.tail, accumulator)
    }

    filterHelper(this, RNil)
  }

  /*
  My Solution
  // Complexity: O(N)
  override def runLengthEncoding: RList[(T, Int)] = {
    def runLengthEncodingHelper(left: RList[T], right: RList[T], accumulator: RList[(T, Int)]): RList[(T, Int)] = {
      if (right.isEmpty)
        accumulator.reverse
      else if (left.isEmpty)
        runLengthEncodingHelper(right.head :: left, right.tail, (right.head, 1) :: accumulator)
      else if (left.head == right.head)
        runLengthEncodingHelper(
          right.head :: left,
          right.tail,
          (accumulator.head._1, accumulator.head._2 + 1) :: accumulator.removeAt(0)
        )
      else
        runLengthEncodingHelper(right.head :: left, right.tail, (right.head, 1) :: accumulator)
    }

    runLengthEncodingHelper(RNil, this, RNil)
  }
   */

  // Daniel's Solution
  override def runLengthEncoding: RList[(T, Int)] = {
    @tailrec
    def runLengthEncodingHelper(
        remaining: RList[T],
        currentTuple: (T, Int),
        accumulator: RList[(T, Int)]
    ): RList[(T, Int)] = {
      if (remaining.isEmpty && currentTuple._2 == 0)
        accumulator
      else if (remaining.isEmpty)
        currentTuple :: accumulator
      else if (remaining.head == currentTuple._1)
        runLengthEncodingHelper(remaining.tail, currentTuple.copy(_2 = currentTuple._2 + 1), accumulator)
      else
        runLengthEncodingHelper(remaining.tail, (remaining.head, 1), currentTuple :: accumulator)
    }
    runLengthEncodingHelper(this.tail, (this.head, 1), RNil).reverse
  }

  /*
  My Solution:
  override def duplicateEach(k: Int): RList[T] = {
    @tailrec
    def duplicateEachHelper(remainingList: RList[T], accumulator: RList[T]): RList[T] = {
      if (remainingList.isEmpty || k == 0)
        accumulator ++ remainingList
      else
        duplicateEachHelper(remainingList.tail, accumulator ++ getDuplicateList(remainingList.head, k))
    }
    duplicateEachHelper(this, RNil)
  }

  def getDuplicateList[S >: T](element: S, frequency: Int): RList[S] = {
    @tailrec
    def getDuplicateHelper(currentFrequency: Int, accumulator: RList[S]): RList[S] = {
      if (currentFrequency == frequency)
        accumulator
      else
        getDuplicateHelper(currentFrequency + 1, element :: accumulator)
    }
    getDuplicateHelper(0, RNil)
  }
   */

  // Daniel's Solution
  // Complexity: O(N * k)
  override def duplicateEach(k: Int): RList[T] = {
    @tailrec
    def duplicateEachHelper(
        remaining: RList[T],
        currentElement: T,
        nDuplications: Int,
        accumulator: RList[T]
    ): RList[T] = {
      if (remaining.isEmpty && nDuplications == k)
        accumulator.reverse
      else if (remaining.isEmpty)
        duplicateEachHelper(remaining, currentElement, nDuplications + 1, currentElement :: accumulator)
      else if (nDuplications == k)
        duplicateEachHelper(remaining.tail, remaining.head, 0, accumulator)
      else
        duplicateEachHelper(remaining, currentElement, nDuplications + 1, currentElement :: accumulator)
    }
    duplicateEachHelper(this.tail, this.head, 0, RNil)
  }

  // Complexity: O(max(N, K)
  override def rotate(k: Int): RList[T] = {
    @tailrec
    def rotateHelper(remaining: RList[T], rotationsLeft: Int, accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty && rotationsLeft == 0)
        this
      else if (remaining.isEmpty)
        rotateHelper(this, rotationsLeft, RNil)
      else if (rotationsLeft == 0)
        remaining ++ accumulator.reverse
      else
        rotateHelper(remaining.tail, rotationsLeft - 1, remaining.head :: accumulator)
    }
    rotateHelper(this, k, RNil)
  }

  // Complexity: O(N * k)
  override def sample(k: Int): RList[T] = {
    val random = new Random(System.currentTimeMillis())
    val maxIndex = this.length

    @tailrec
    def sampleHelper(nRemaining: Int, accumulator: RList[T]): RList[T] = {
      if (nRemaining == 0)
        accumulator
      else
        sampleHelper(nRemaining - 1, this(random.nextInt(maxIndex)) :: accumulator)
    }

    if (k < 0)
      RNil
    else
      sampleHelper(k, RNil)
  }

}

object RList {

  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def fromHelper(it: Iterable[T], accumulator: RList[T]): RList[T] = {
      if (it.isEmpty)
        accumulator
      else
        fromHelper(it.tail, it.head :: accumulator)
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

  val list1 = 1 :: 2 :: 3 :: RNil
  val list2 = 4 :: 5 :: 6 :: RNil
  println(list1 ++ list2) // [1, 2, 3, 4, 5, 6]

  println(list.removeAt(0)) // [2, 3, 9, 8, 7]
  println(list.removeAt(1)) // [1, 3, 9, 8, 7]
  println(list.removeAt(2)) // [1, 2, 9, 8, 7]

  // println(list.removeAt(10))
  // NoSuchElementException

  println(list.map(_ * 2)) // [2, 4, 6, 18, 16, 14]
  println(list.flatMap(x => x :: x + 1 :: RNil)) // [1, 2, 2, 3, 3, 4, 9, 10, 8, 9, 7, 8]
  println(list.filter(_ % 2 == 0)) // [2, 8]

  val listRLE = 1 :: 1 :: 2 :: 3 :: 3 :: 3 :: 3 :: 3 :: 4 :: 4 :: 4 :: 5 :: 6 :: RNil
  println(listRLE.runLengthEncoding) // [(1,2), (2,1), (3,5), (4,3), (5,1), (6,1)]

  println(list.duplicateEach(2)) // [1, 1, 2, 2, 3, 3, 9, 9, 8, 8, 7, 7]

  val list3 = 1 :: 2 :: 3 :: RNil

  println(list3.rotate(2)) // [3, 1, 2]
  println(list3.rotate(3)) // [1, 2, 3]
  println(list3.rotate(6)) // [1, 2, 3]

  println(list.sample(3)) // [1, 3, 7]
}
