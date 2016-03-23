package main.scala.chapter2

import scala.annotation.tailrec

// Sealed implies all implementations of List have to be declared in this file
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

	def sum(integers: List[Int]): Int = integers match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1
		case Cons(0.0, xs) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	/**
	 * Variadic arguments accept zero or more arguments of type A. it is common for companion objects of container types
	 * to have variadic apply functions that construct the data type.Variadic functions are just providing a little syntax sugar
	 * for creating and passing a Seq of elements explicitly. We can convert a List[A]to variadic parameters by invoking
	 * List: _*. So if we want to call apply on List(1,2,3), we call apply(List(1,2,3): _*)
	 */
	def apply[A](as: A*): List[A] = { // This * syntax is called variadic parameters syntax
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*)) //converts to variable arguments
	}

	// Exercise 1
	val listPatternMatching = List(1, 2, 3, 4, 5) match {
		case Cons(x, Cons(2, Cons(4, _))) => x
		case Nil => 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // This is the case that will match
		case Cons(h, t) => h + sum(t)
		case _ => 101
	}

	// Exercise 2
	def tail[A](list: List[A]): List[A] = {
		list match {
			case Cons(head, tail) => tail
			case Nil => Nil
		}
	}

	def head[A](list: List[A]): A = {
		list match {
			case Cons(head, tail) => head
			case Nil => throw new RuntimeException("Head not defined for this list")
		}
	}

	//Exercise 3
	def drop[A](list: List[A], n: Int): List[A] = {
		def innerDrop(droppedList: List[A], dropped: Int): List[A] = {
			if (n <= dropped)
				droppedList
			else innerDrop(tail(droppedList), dropped + 1)
		}
		innerDrop(list, 0)
	}

	// Exercise 4
	def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
		def innerDrop(droppedList: List[A], dropped: Int): List[A] = {
			if (!f(head(droppedList)))
				droppedList
			else innerDrop(tail(droppedList), dropped + 1)
		}
		innerDrop(l, 0)
	}

	// Exercise 5
	def setHead[A](list: List[A], newHead: A): List[A] = {
		list match {
			case Cons(head, tail) => Cons(newHead, tail)
			case Nil => Cons(newHead, Nil)
		}
	}

	def append[A](listOne: List[A], listTwo: List[A]): List[A] = {
		listOne match {
			case Nil => listTwo
			case Cons(head, tail) => Cons(head, append(tail, listTwo))
		}
	}
	// Exercise 6
	def init[A](l: List[A]): List[A] = {
		???
	}

	def foldRight[A, B](list: List[A], initialValue: B)(f: (A, B) => B): B = {
		list match {
			case Nil => initialValue
			case Cons(head, tail) => foldRight(tail, f(head, initialValue))(f)
		}
	}

	def sumFoldRight(integers: List[Int]): Int = {
		foldRight(integers, 0)(_ + _)
	}

	def productFoldRight(integers: List[Int]): Int = {
		foldRight(integers, 1)(_ * _)
	}

	// Exercise 7
	def productFoldRightWithShortCircuit(integers: List[Int]): Int = {
		foldRight(integers, 1)((x, y) => {
			if (x == 0 || y == 0) return 0 // using a return isn't idiomatic Scala, but it does short circuit
			else x * y
		})
	}

	// Exercise 9
	def length[A](list: List[A]): Int = {
		foldRight(list, 0)((x, y) =>
			1 + y)
	}

	// Exercise 10
	def foldLeft[A, B](l: List[A], initialValue: B)(f: (B, A) => B): B = {
		@tailrec
		def foldLeftInner(result: B, list: List[A]): B = {
			list match {
				case Nil => result
				case Cons(head, tail) => foldLeftInner(f(result, head), tail)
			}
		}
		foldLeftInner(initialValue, l)
	}

	// Exercise 11
	def sumUsingFoldLeft(integers: List[Int]): Int = {
		foldLeft(integers, 0)(_ + _)
	}

	def productUsingFoldLeft(integers: List[Int]): Int = {
		foldLeft(integers, 1)(_ * _)
	}

	def lengthUsingFoldLeft(integers: List[Int]): Int = {
		foldLeft(integers, 0)((x, y) => y + 1)
	}

	// Exercise 12
	def reverse[A](integers: List[A]): List[A] = {
		foldRight(integers, Nil: List[A])((x, y) => Cons(x, y))
	}

}
