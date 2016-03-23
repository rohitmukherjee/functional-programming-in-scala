package main.scala.chapter2

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	// Exercise 25
	def size[A](tree: Tree[A]): Int = {
		tree match {
			case leaf: Leaf[A] => 1
			case branch: Branch[A] => size(branch.left) + size(branch.right) + 1
		}
	}

	// Exercise 26
	def maximum(tree: Tree[Int]): Int = {
		tree match {
			case Leaf(value) => value
			case Branch(left, right) => maximum(left) max maximum(right)
		}
	}

	// Exercise 27
	def depth[A](tree: Tree[A]): Int = {
		def depthInner(currentTree: Tree[A], d: Int): Int = {
			currentTree match {
				case Leaf(_) => d + 1
				case Branch(left, right) => depthInner(left, d)
			}
		}
		???
	}
}