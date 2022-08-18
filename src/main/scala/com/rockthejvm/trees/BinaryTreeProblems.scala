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
  def size: Int

  def collectNodes(level: Int): List[BTree[T]]

  def mirror: BTree[T]

  def sameShapeAs[S >: T](that: BTree[S]): Boolean

  def isSymmetrical: Boolean
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

  override def collectNodes(level: Int): List[BTree[Nothing]] = List()

  override def mirror: BTree[Nothing] = BEnd

  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty

  override def isSymmetrical: Boolean = true
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

  override def collectNodes(level: Int): List[BTree[T]] = {
    /*
                _____1_____
               /           \
             __2__       __6__
            /     \     /     \
            3     4     7     8
                   \
                    5
           level = 2
           collectNodesHelper(0, [{1}])
           = collectNodesHelper(1, [{2}, {6}])
           = collectNodesHelper(2, [{3}, {4}, {7}, {8}])
           = [{3}, {4}, {7}, {8}]
     */

    @tailrec
    def collectNodesHelper(currentLevel: Int, currentNodes: List[BTree[T]]): List[BTree[T]] = {
      if (currentNodes.isEmpty)
        List()
      else if (currentLevel == level)
        currentNodes
      else {
        val expandedNodes =
          for {
            node <- currentNodes
            child <- List(node.left, node.right) if !child.isEmpty
          } yield child

        collectNodesHelper(currentLevel + 1, expandedNodes)
      }
    }
    if (level < 0)
      List()
    else
      collectNodesHelper(0, List(this))
  }

  override def mirror: BTree[T] = {
    /*
            _____1_____                     _____1_____
           /           \                   /           \
         __2__       __6__       ->      __6__       __2__
        /     \     /     \             /     \     /     \
        3     4     7     8             8     7     4     3
               \                                   /
                5                                 5
        mt([1], [], []) =
        mt([2,6,1], [1], []) =
        mt([3,4,2,6,1], [1,2], []) =
        mt([4,2,6,1], [1,2], [3]) =
        mt([End, 5, 4,2,6,1], [1,2,4], [3]) =
        mt([5,4,2,6,1], [1,2,4], [End, 3]) =
        mt([4,2,6,1], [1,2,4], [5, End, 3]) =
        mt([2,6,1], [1,2,4], [(4 5 End), 3]) =
        mt([6,1], [1,2,4], [(2 (4 5 End) 3)] =
        mt([7,8,6,1], [1,2,4,6], [(2 (4 5 End) 3)]) =
        mt([8,6,1], [1,2,4,6], [7, (2 (4 5 End) 3)]) =
        mt([6,1], [1,2,4,6], [8,7, (2 (4 5 End) 3)]) =
        mt([1], [1,2,4,6], [(6 8 7), (2 (4 5 End) 3)]) =
        mt([], [1,2,4,6], [(1 (6 8 7) (2 (4 5 End) 3)]) =
        (1 (6 8 7) (2 (4 5 End) 3)

        Complexity: O(N)
     */
    @tailrec
    def mirrorHelper(remaining: List[BTree[T]], expanded: Set[BTree[T]], done: List[BTree[T]]): BTree[T] = {
      if (remaining.isEmpty)
        done.head
      else {
        val node = remaining.head
        if (node.isEmpty || node.isLeaf)
          mirrorHelper(remaining.tail, expanded, node :: done)
        else if (!expanded.contains(node))
          mirrorHelper(node.left :: node.right :: remaining, expanded + node, done)
        else {
          val newLeft = done.head
          val newRight = done.tail.head
          val newNode = BNode(node.value, newLeft, newRight)

          mirrorHelper(remaining.tail, expanded, newNode :: done.drop(2))
        }
      }
    }

    mirrorHelper(List(this), Set(), List())
  }

  /*
          _____1_____                     _____8_____
         /           \                   /           \
       __2__       __6__       ~~      __9__       __2__
      /     \     /     \             /     \     /     \
      3     4     7     8             1     3     2     7
             \                               \
              5                               4

          sst([1], [8]) =
          sst([2,6], [9,2]) =
          sst([3,4,6], [1,3,2]) =
          sst([4,6],[3,2]) =
          sst([End, 5, 6], [End, 4, 2]) =
          sst([5,6], [4,2]) =
          sst([6], [2]) =
          sst([7,8], [2,7]) =
          sst([8], [7]) =
          sst([], []) =
          true

          Complexity: O(max(N1, N2))
   */
  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = {
    @tailrec
    def sameShapeAsHelper(thisRemaining: List[BTree[S]], thatRemaining: List[BTree[S]]): Boolean = {
      if (thisRemaining.isEmpty)
        thatRemaining.isEmpty
      else if (thatRemaining.isEmpty)
        thisRemaining.isEmpty
      else {
        val thisNode = thisRemaining.head
        val thatNode = thatRemaining.head

        if (thisNode.isEmpty)
          thatNode.isEmpty && sameShapeAsHelper(thisRemaining.tail, thatRemaining.tail)
        else if (thisNode.isLeaf)
          thatNode.isLeaf && sameShapeAsHelper(thisRemaining.tail, thatRemaining.tail)
        else
          sameShapeAsHelper(
            thisNode.left :: thisNode.right :: thisRemaining.tail,
            thatNode.left :: thatNode.right :: thatRemaining.tail
          )
      }
    }

    sameShapeAsHelper(List(this), List(that))
  }

  override def isSymmetrical: Boolean = sameShapeAs(this.mirror)
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

  println(tree.collectNodes(2).map(_.value)) // List(3, 4, 7, 8)
  println(tree.collectNodes(3).map(_.value)) // List(5)

  println(
    tree.mirror
  ) // BNode(1,BNode(6,BNode(8,BEnd,BEnd),BNode(7,BEnd,BEnd)),BNode(2,BNode(4,BNode(5,BEnd,BEnd),BEnd),BNode(3,BEnd,BEnd)))

  val tree10x = BNode(
    10,
    BNode(20, BNode(30, BEnd, BEnd), BNode(40, BEnd, BNode(50, BEnd, BEnd))),
    BNode(60, BNode(70, BEnd, BEnd), BNode(80, BEnd, BEnd))
  )

  println(tree.sameShapeAs(tree10x)) // true

  val tree10xExtra = BNode(
    10,
    BNode(20, BNode(30, BEnd, BEnd), BNode(40, BEnd, BEnd)),
    BNode(60, BNode(70, BEnd, BEnd), BNode(80, BEnd, BEnd))
  )

  println(tree10xExtra.isSymmetrical) // true
}
