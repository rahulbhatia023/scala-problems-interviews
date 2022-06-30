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

  def flatMapImproved[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]

  def runLengthEncoding: RList[(T, Int)]

  def duplicateEach(k: Int): RList[T]

  def rotate(k: Int): RList[T]

  def sample(k: Int): RList[T]

  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]

  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
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

  override def flatMapImproved[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  override def runLengthEncoding: RList[(Nothing, Int)] = RNil

  override def duplicateEach(k: Int): RList[Nothing] = RNil

  override def rotate(k: Int): RList[Nothing] = RNil

  override def sample(k: Int): RList[Nothing] = RNil

  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil

  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
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

  /*
        [1,2,3,4,5].apply(2) = applyHelper([1,2,3,4,5], 0)
        = applyHelper([2,3,4,5], 1)
        = applyHelper([3,4,5], 2)
        = 3

        Complexity: O(min(N, index))
   */
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

  /*
        [1,2,3,4,5].length = lengthHelper([1,2,3,4,5], 0)
        = lengthHelper([2,3,4,5], 1)
        = lengthHelper([3,4,5], 2)
        = lengthHelper([4,5], 3)
        = lengthHelper([5], 4)
        = lengthHelper([], 5)
        = 5

        Complexity: O(N)
   */
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

  /*
        [1,2,3,4].reverse = reverseHelper([1,2,3,4], RNil)
        = reverseHelper([2,3,4], [1])
        = reverseHelper([3,4], [2,1])
        = reverseHelper([4], [3,2,1])
        = reverseHelper([], [4,3,2,1])
        = [4,3,2,1]

        Complexity: O(N)
   */
  override def reverse: RList[T] = {
    @tailrec
    def reverseHelper(list: RList[T], accumulator: RList[T]): RList[T] = {
      if (list.isEmpty)
        accumulator
      else
        reverseHelper(list.tail, list.head :: accumulator)
    }

    reverseHelper(this, RNil)
  }

  /*
        Concatenation

        [1,2,3] ++ [4,5] = concatHelper([3,2,1], [4,5])
        = concatHelper([2,1], [3,4,5])
        = concatHelper([1], [2,3,4,5])
        = concatHelper([], [1,2,3,4,5])
        = [1,2,3,4,5]

        Complexity: O(M + N), where M and N are the length of respective lists
   */
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

  /*
        [1,2,3,4,5].removeAt(2) = removeAtHelper([1,2,3,4,5], 0, [])
        = removeAtHelper([2,3,4,5], 1, [1])
        = removeAtHelper([3,4,5], 2, [2,1])
        = [2,1].reverse ++ [4,5]

        Complexity: O(N)
   */
  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtHelper(left: RList[T], currentIndex: Int, right: RList[T]): RList[T] = {
      if (currentIndex == index)
        right.reverse ++ left.tail
      else
        removeAtHelper(left.tail, currentIndex + 1, left.head :: right)
    }

    if (index < 0)
      this
    else
      removeAtHelper(this, 0, RNil)
  }

  /*
        [1,2,3].map(x => x + 1) = mapHelper([1,2,3], [])
        = mapHelper([2,3], [2])
        = mapHelper([3], [3, 2])
        = mapHelper([], [4,3,2])
        = [4,3,2].reverse
        = [2,3,4]

        Complexity: O(N)
   */
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

  /*
        [1,2,3].flatMap(x => [x, 2 * x]) = flatMapHelper([1,2,3], [])
        = flatMapHelper([2,3], [] ++ [1,2])
        = flatMapHelper([3], [1,2] ++ [2,4])
        = flatMapHelper([], [1,2] ++ [2,4] ++ [3,6])
        = [1,2,2,4,3,6]

        Complexity: O(Z^2)
   */
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

  /*
        [1,2,3].flatMap(x => [x, 2 * x]) = flatMapImprovedHelper([1,2,3], [])
        = flatMapImprovedHelper([2,3], [[2,1]])
        = flatMapImprovedHelper([3], [[4,2], [2,1]])
        = flatMapImprovedHelper([], [[6,3], [4,2], [2,1]])
        = concatenateAll([[6,3], [4,2], [2,1]], [], [])
        = concatenateAll([[4,2], [2,1]], [6,3], [])
        = concatenateAll([[4,2], [2,1]], [3], [6])
        = concatenateAll([[4,2], [2,1]], [], [3,6])
        = concatenateAll([[2,1]], [4,2], [3,6])
        = concatenateAll([[2,1]], [2], [4,3,6])
        = concatenateAll([[2,1]], [], [2,4,3,6])
        = concatenateAll([], [2,1], [2,4,3,6])
        = concatenateAll([], [1], [2,2,4,3,6])
        = concatenateAll([], [], [1,2,2,4,3,6])
        = [1,2,2,4,3,6]

        Complexity: O(N + Z) where N -> length of initial list, Z -> length of final list
   */
  override def flatMapImproved[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapImprovedHelper(remaining: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty)
        concatenateAll(accumulator, RNil, RNil)
      else
        flatMapImprovedHelper(remaining.tail, f(remaining.head).reverse :: accumulator)
    }

    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], accumulator: RList[S]): RList[S] = {
      if (elements.isEmpty && currentList.isEmpty)
        accumulator
      else if (currentList.isEmpty)
        concatenateAll(elements.tail, elements.head, accumulator)
      else
        concatenateAll(elements, currentList.tail, currentList.head :: accumulator)
    }

    flatMapImprovedHelper(this, RNil)
  }

  /*
        [1,2,3,4,5].filter(x => x % 2 == 0) = filterHelper([1,2,3,4,5], [])
        = filterHelper([2,3,4,5], [])
        = filterHelper([3,4,5], [2])
        = filterHelper([4,5], [2])
        = filterHelper([5], [4,2])
        = filterHelper([], [4,2])
        = [2,4]

        Complexity: O(N)
   */
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
        [1,1,1,2,2,3,4,4,4,5].rle = runLengthEncodingHelper([1,1,2,2,3,4,4,4,5], (1, 1), []) =
        = runLengthEncodingHelper([1,2,2,3,4,4,4,5], (1,2), [])
        = runLengthEncodingHelper([2,2,3,4,4,4,5], (1,3), [])
        = runLengthEncodingHelper([2,3,4,4,4,5], (2,1), [(1,3)])
        = runLengthEncodingHelper([3,4,4,4,5], (2,2), [(1,3)])
        = runLengthEncodingHelper([4,4,4,5], (3,1), [(2,2), (1,3)]
        = ...
        = [(5,1), (4,3), (3,1), (2,2), (1,3)].reverse
        = [(1,3), (2,2), (3,1), (4,3), (5,1)]

        Complexity: O(N)
   */
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
        [1,2].duplicateEach(3) = duplicateEachHelper([2], 1, 0, [])
        = duplicateEachHelper([2], 1, 1, [1])
        = duplicateEachHelper([2], 1, 2, [1,1])
        = duplicateEachHelper([2], 1, 3, [1,1,1])
        = duplicateEachHelper([], 2, 0, [1,1,1])
        = duplicateEachHelper([], 2, 1, [2,1,1,1])
        = duplicateEachHelper([], 2, 2, [2,2,1,1,1])
        = duplicateEachHelper([], 2, 3, [2,2,2,1,1,1])
        = [2,2,2,1,1,1].reverse
        = [1,1,1,2,2,2]

        Complexity: O(N * K)
   */
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

  /*
        [1,2,3].rotate(3) == [1,2,3]
        [1,2,3].rotate(6) == [1,2,3]
        [1,2,3].rotate(4) == [1,2,3].rotate(1)

        [1,2,3].rotate(1) = rotateHelper([1,2,3], 1, [])
        = rotateHelper([2,3], 0, [1])
        = [2,3,1]

        [1,2,3].rotate(3) = rotateHelper([1,2,3], 3, [])
        = rotateHelper([2,3], 2, [1])
        = rotateHelper([3], 1, [2,1])
        = rotateHelper([], 0, [3,2,1])
        = [1,2,3]

        [1,2,3].rotate(4) = rotateHelper([1,2,3], 4, [])
        = rotateHelper([2,3], 3, [1])
        = rotateHelper([3], 2, [2,1])
        = rotateHelper([], 1, [3,2,1])
        = rotateHelper([1,2,3], 1, [])
        = rotateHelper([2,3], 0, [1])
        = [2,3,1]

        Complexity: O(max(N, K))
   */
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

  /*
        [1,2,3,4,5].sample(3) = sampleHelper(3, [])
        = sampleHelper(2, [2])
        = sampleHelper(1, [4,2])
        = sampleHelper(0, [4,4,2])
        = [4,4,2]

        Complexity: O(N * K)
   */
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

  // Insertion Sort
  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
        [3,1,4,2,5].insertionSort = insertionSortHelper([3,1,4,2,5], []) =
          = insertionSortHelper([1,4,2,5], [3])
          = insertionSortHelper([4,2,5], [1,3])
          = insertionSortHelper([2,5], [1,3,4])
          = insertionSortHelper([5], [1,2,3,4])
          = insertionSortHelper([], [1,2,3,4,5])
          = [1,2,3,4,5]

          Complexity: O(N^2)
     */

    @tailrec
    def insertionSortHelper(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty)
        accumulator
      else
        insertionSortHelper(remaining.tail, insertSorted(remaining.head, RNil, accumulator))
    }

    /*
        insertSorted(4, [], [1,2,3,5]) =
        insertSorted(4, [1], [2,3,5]) =
        insertSorted(4, [2,1], [3,5]) =
        insertSorted(4, [3,2,1], [5]) =
        [3,2,1].reverse + (4 :: [5]) =
        [1,2,3,4,5]

        Complexity: O(N)
     */
    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S]): RList[S] = {
      if (after.isEmpty || ordering.lteq(element, after.head))
        before.reverse ++ (element :: after)
      else
        insertSorted(element, after.head :: before, after.tail)
    }

    insertionSortHelper(this, RNil)
  }

  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {

    /*
          [3,1,2,5,4] => [[3],[1],[2],[5],[4]]

          mergeSortHelper([[3],[1],[2],[5],[4]], []) =
          = mergeSortHelper([[2],[5],[4]], [[1,3]])
          = mergeSortHelper([[4]], [[2,5], [1,3]])
          = mergeSortHelper([], [[4], [2,5], [1,3]]) =
          = mergeSortHelper([[4], [2,5], [1,3]], [])
          = mergeSortHelper([[1,3]], [[2,4,5]])
          = mergeSortHelper([], [[1,3], [2,4,5]])
          = mergeSortHelper([[1,3], [2,4,5]], [])
          = mergeSortHelper([], [[1,2,3,4,5]])
          = [1,2,3,4,5]

          Complexity: O(n * log(n))
     */
    @tailrec
    def mergeSortHelper(smallLists: RList[RList[S]], bigLists: RList[RList[S]]): RList[S] = {
      if (smallLists.isEmpty) {
        if (bigLists.isEmpty)
          RNil
        else if (bigLists.tail.isEmpty)
          bigLists.head
        else
          mergeSortHelper(bigLists, RNil)
      } else if (smallLists.tail.isEmpty) {
        if (bigLists.isEmpty)
          smallLists.head
        else
          mergeSortHelper(smallLists.head :: bigLists, RNil)
      } else {
        val firstList = smallLists.head
        val secondList = smallLists.tail.head
        val mergedList = merge(firstList, secondList, RNil)

        mergeSortHelper(smallLists.tail.tail, mergedList :: bigLists)
      }
    }

    /*
          merge([1,3], [2,4,5,6,7], []) =
          merge([3], [2,4,5,6,7], [1]) =
          merge([3], [4,5,6,7], [2,1]) =
          merge([], [4,5,6,7], [3,2,1]) =
          [1,2,3] ++ [4,5,6,7] =
          [1,2,3,4,5,6,7]
     */
    @tailrec
    def merge(firstList: RList[S], secondList: RList[S], accumulator: RList[S]): RList[S] = {
      if (firstList.isEmpty)
        accumulator.reverse ++ secondList
      else if (secondList.isEmpty)
        accumulator.reverse ++ firstList
      else if (ordering.lteq(firstList.head, secondList.head))
        merge(firstList.tail, secondList, firstList.head :: accumulator)
      else
        merge(firstList, secondList.tail, secondList.head :: accumulator)
    }

    mergeSortHelper(this.map(x => x :: RNil), RNil)
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
  println(list.flatMapImproved(x => x :: x + 1 :: RNil)) // [1, 2, 2, 3, 3, 4, 9, 10, 8, 9, 7, 8]
  println(list.filter(_ % 2 == 0)) // [2, 8]

  val listRLE = 1 :: 1 :: 2 :: 3 :: 3 :: 3 :: 3 :: 3 :: 4 :: 4 :: 4 :: 5 :: 6 :: RNil
  println(listRLE.runLengthEncoding) // [(1,2), (2,1), (3,5), (4,3), (5,1), (6,1)]

  println(list.duplicateEach(2)) // [1, 1, 2, 2, 3, 3, 9, 9, 8, 8, 7, 7]

  val list3 = 1 :: 2 :: 3 :: RNil

  println(list3.rotate(2)) // [3, 1, 2]
  println(list3.rotate(3)) // [1, 2, 3]
  println(list3.rotate(6)) // [1, 2, 3]

  println(list.sample(3)) // [1, 3, 7]

  val list4 = 3 :: 1 :: 2 :: 4 :: 5 :: RNil
  val ordering = Ordering.fromLessThan[Int](_ < _)
  println(list4.insertionSort(ordering)) // [1, 2, 3, 4, 5]

  println(list4.mergeSort(ordering)) // [1, 2, 3, 4, 5]
}
