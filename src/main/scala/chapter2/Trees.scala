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
		tree match {
			case leaf: Leaf[A] => 1
			case Branch(left, right) => 1 + (depth(left) max depth(right))
		}
	}

	// Exercise 28 
	def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
		tree match {
			case leaf: Leaf[A] => Leaf(f(leaf.value))
			case branch: Branch[A] => Branch(map(branch.right)(f), map(branch.left)(f))
		}
	}

	/*
	 * Exercise 29 - General purpose fold function that can be used to implement size/depth etc.
	 * Just traverses over the tree in
	 */
	def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = {
		tree match {
			case leaf: Leaf[A] => f(leaf.value, z)
			case branch: Branch[A] => ???
		}
	}

}