package com.rockthejvm.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T

  def left: BTree[T]

  def right: BTree[T]

  def isEmpty: Boolean

  def isLeaf: Boolean

  def collectLeaves: List[BTree[T]]

  def leafCount: Int

  // number of nodes in the tree
  val size: Int
}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException

  override def left: BTree[Nothing] = throw new NoSuchElementException

  override def right: BTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[BTree[Nothing]] = List()

  override def leafCount: Int = 0

  override val size: Int = 0
}

case class BNode[+T](override val value: T, override val left: BTree[T], override val right: BTree[T])
    extends BTree[T] {
  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  /*
          _____1_____
         /           \
       __2__       __6__
      /     \     /     \
      3     4     7     8
             \
              5

         clt([1], []) =
         clt([2, 6], []) =
         clt([3,4,6], []) =
         clt([4,6], [3]) =
         clt([5,6], [3]) =
         clt([6], [5,3]) =
         clt([7,8], [5,3]) =
         clt([8], [7,5,3]) =
         clt([], [8,7,5,3]) =
         [8,7,5,3]
   */
  override def collectLeaves: List[BTree[T]] = {
    @tailrec
    def collectLeavesHelper(todo: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] = {
      if (todo.isEmpty)
        leaves
      else if (todo.head.isEmpty)
        collectLeavesHelper(todo.tail, leaves)
      else if (todo.head.isLeaf)
        collectLeavesHelper(todo.tail, todo.head :: leaves)
      else {
        val node = todo.head
        collectLeavesHelper(node.left :: node.right :: todo.tail, leaves)
      }
    }

    collectLeavesHelper(List(this), List())
  }

  override def leafCount: Int = collectLeaves.length

  override val size: Int = 1 + left.size + right.size
}

object BinaryTreeProblems extends App {

  val tree = BNode(
    1,
    BNode(2, BNode(3, BEnd, BEnd), BNode(4, BEnd, BNode(5, BEnd, BEnd))),
    BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
  )

  println(tree.collectLeaves.map(_.value)) // List(8, 7, 5, 3)
  println(tree.leafCount) // 4

  val bigTree = (1 to 10000).foldLeft[BTree[Int]](BEnd) { (tree, number) => BNode(number, tree, BEnd) }
  println(bigTree.size) // 10000
}
