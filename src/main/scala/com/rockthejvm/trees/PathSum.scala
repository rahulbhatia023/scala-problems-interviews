package com.rockthejvm.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PathSum extends App {

  /*
              _____1_____
             /           \
           __2__       __6__
          /     \     /     \
          3     4     7     8
                 \
                  5
          tree, 6 => true
          tree, 7 => false
   */
  // Return true if there is a path from root to a leaf, such that the sum of values is target.
  def hasPathSum(tree: Tree[Int], target: Int) = {
    @tailrec
    def hasPathSumHelper1(tree: Tree[Int], target: Int): Boolean = {
      if (tree.isEmpty)
        target == 0
      else if (tree.isLeaf)
        target == tree.value
      else if (tree.left.isEmpty)
        hasPathSumHelper1(tree.right, target - tree.value)
      else
        hasPathSumHelper1(tree.left, target - tree.value)
    }

    @tailrec
    def hasPathSumHelper2(nodes: Queue[Tree[Int]], targets: Queue[Int]): Boolean = {
      if (nodes.isEmpty)
        false
      else {
        val node = nodes.head
        val targetValue = targets.head
        val children = List(node.left, node.right).filter(!_.isEmpty)
        val childrenTargets = children.map(_ => targetValue - node.value)

        if (node.isLeaf && node.value == targetValue)
          true
        else
          hasPathSumHelper2(nodes.tail ++ children, targets.tail ++ childrenTargets)
      }
    }

    hasPathSumHelper1(tree, target)
    hasPathSumHelper2(Queue(tree), Queue(target))
  }

  def findSumPaths(tree: Tree[Int], target: Int): List[List[Int]] = {
    /*
                _____1_____
               /           \
             __2__       __6__
            /     \     /     \
            3     4     7     8
                   \
                    -1
           sp(1, 6) = [2 6].flatMap(f) = [[2 3] [2 4 -1]].map(path => 1 :: path) = [[1 2 3] [1 2 4 -1]]
             sp(2, 5) = [3, 4].flatMap(f) == [[3]].map(path => 2 :: path) ++ [[4 -1]].map(path => 2 :: path) = [[2 3] [2 4 -1]]
               sp(3, 3) = [[3]]
               sp(4, 3) = [[-1]].map(path => 4 :: path) = [[4, -1]]
                 sp(-1, -1) = [[-1]]
             sp(6, 5) = []
     */
    def stackPaths(tree: Tree[Int], currentTarget: Int): List[List[Int]] = {
      if (tree.isEmpty)
        List()
      else if (tree.isLeaf) {
        if (tree.value == currentTarget)
          List(List(tree.value))
        else
          List()
      } else
        List(tree.left, tree.right)
          .filter(!_.isEmpty)
          .flatMap { childNode =>
            stackPaths(childNode, currentTarget - tree.value)
              .map(path => tree.value :: path)
          }
    }

    stackPaths(tree, target)
  }

  val tree = Node(
    1,
    Node(2, Node(3, End, End), Node(4, End, Node(5, End, End))),
    Node(6, Node(7, End, End), Node(8, End, End))
  )

  println(hasPathSum(tree, 6)) // true
  println(hasPathSum(tree, 7)) // false
}
