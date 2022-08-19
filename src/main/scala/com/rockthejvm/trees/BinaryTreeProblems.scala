package com.rockthejvm.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed abstract class Tree[+T] {
  def value: T

  def left: Tree[T]

  def right: Tree[T]

  def isEmpty: Boolean

  def isLeaf: Boolean

  def collectLeaves: List[Tree[T]]

  def leafCount: Int

  // number of nodes in the tree
  def size: Int

  def collectNodes(level: Int): List[Tree[T]]

  def mirror: Tree[T]

  def sameShapeAs[S >: T](that: Tree[S]): Boolean

  def isSymmetrical: Boolean

  def toList: List[T]
}

case object End extends Tree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException

  override def left: Tree[Nothing] = throw new NoSuchElementException

  override def right: Tree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[Tree[Nothing]] = List()

  override def leafCount: Int = 0

  override val size: Int = 0

  override def collectNodes(level: Int): List[Tree[Nothing]] = List()

  override def mirror: Tree[Nothing] = End

  override def sameShapeAs[S >: Nothing](that: Tree[S]): Boolean = that.isEmpty

  override def isSymmetrical: Boolean = true

  override def toList: List[Nothing] = List()
}

case class Node[+T](override val value: T, override val left: Tree[T], override val right: Tree[T]) extends Tree[T] {
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
  override def collectLeaves: List[Tree[T]] = {
    @tailrec
    def collectLeavesHelper(todo: List[Tree[T]], leaves: List[Tree[T]]): List[Tree[T]] = {
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

  override def collectNodes(level: Int): List[Tree[T]] = {
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
    def collectNodesHelper(currentLevel: Int, currentNodes: List[Tree[T]]): List[Tree[T]] = {
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

  override def mirror: Tree[T] = {
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
    def mirrorHelper(remaining: List[Tree[T]], expanded: Set[Tree[T]], done: List[Tree[T]]): Tree[T] = {
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
          val newNode = Node(node.value, newLeft, newRight)

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
  override def sameShapeAs[S >: T](that: Tree[S]): Boolean = {
    @tailrec
    def sameShapeAsHelper(thisRemaining: List[Tree[S]], thatRemaining: List[Tree[S]]): Boolean = {
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

  override def toList: List[T] = {
    @tailrec
    def preOrderTraversal(stack: List[Tree[T]], visited: Set[Tree[T]], accumulator: Queue[T]): List[T] = {
      if (stack.isEmpty)
        accumulator.toList
      else {
        val node = stack.head

        if (node.isEmpty)
          preOrderTraversal(stack.tail, visited, accumulator)
        else if (node.isLeaf || visited.contains(node))
          preOrderTraversal(stack.tail, visited, accumulator :+ node.value)
        else
          preOrderTraversal(node :: node.left :: node.right :: stack.tail, visited + node, accumulator)
      }
    }

    @tailrec
    def inOrderTraversal(stack: List[Tree[T]], visited: Set[Tree[T]], accumulator: Queue[T]): List[T] = {
      if (stack.isEmpty)
        accumulator.toList
      else {
        val node = stack.head

        if (node.isEmpty)
          inOrderTraversal(stack.tail, visited, accumulator)
        else if (node.isLeaf || visited.contains(node))
          inOrderTraversal(stack.tail, visited, accumulator :+ node.value)
        else
          inOrderTraversal(node.left :: node :: node.right :: stack.tail, visited + node, accumulator)
      }
    }

    @tailrec
    def postOrderTraversal(stack: List[Tree[T]], visited: Set[Tree[T]], accumulator: Queue[T]): List[T] = {
      if (stack.isEmpty)
        accumulator.toList
      else {
        val node = stack.head

        if (node.isEmpty)
          postOrderTraversal(stack.tail, visited, accumulator)
        else if (node.isLeaf || visited.contains(node))
          postOrderTraversal(stack.tail, visited, accumulator :+ node.value)
        else
          postOrderTraversal(node.left :: node.right :: node :: stack.tail, visited + node, accumulator)
      }
    }

    @tailrec
    def perLevelTraversal(level: List[Tree[T]], accumulator: Queue[Tree[T]]): List[T] = {
      if (level.isEmpty)
        accumulator.map(_.value).toList
      else
        perLevelTraversal(
          level.flatMap(node => List(node.left, node.right).filter(!_.isEmpty)),
          accumulator ++ level
        )
    }

    // preOrderTraversal(List(this), Set(), Queue())
    // inOrderTraversal(List(this), Set(), Queue())
    // postOrderTraversal(List(this), Set(), Queue())
    perLevelTraversal(List(this), Queue())
  }

}

object BinaryTreeProblems extends App {

  val tree = Node(
    1,
    Node(2, Node(3, End, End), Node(4, End, Node(5, End, End))),
    Node(6, Node(7, End, End), Node(8, End, End))
  )

  println(tree.collectLeaves.map(_.value)) // List(8, 7, 5, 3)
  println(tree.leafCount) // 4

  val bigTree = (1 to 10000).foldLeft[Tree[Int]](End) { (tree, number) => Node(number, tree, End) }
  println(bigTree.size) // 10000

  println(tree.collectNodes(2).map(_.value)) // List(3, 4, 7, 8)
  println(tree.collectNodes(3).map(_.value)) // List(5)

  println(
    tree.mirror
  ) // Node(1,Node(6,Node(8,End,End),Node(7,End,End)),Node(2,Node(4,Node(5,End,End),End),Node(3,End,End)))

  val tree10x = Node(
    10,
    Node(20, Node(30, End, End), Node(40, End, Node(50, End, End))),
    Node(60, Node(70, End, End), Node(80, End, End))
  )

  println(tree.sameShapeAs(tree10x)) // true

  val tree10xExtra = Node(
    10,
    Node(20, Node(30, End, End), Node(40, End, End)),
    Node(60, Node(70, End, End), Node(80, End, End))
  )

  println(tree10xExtra.isSymmetrical) // true

  println(tree.toList) // List(1, 2, 3, 4, 5, 6, 7, 8)
}
