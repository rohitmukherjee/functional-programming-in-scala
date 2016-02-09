package main.scala.chapter2

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

	//Exercise 3
	def drop[A](list: List[A], n: Int): List[A] = {
		???
	}

	//Test Functions
	def tailTest = {
		println(tail(List(1, 2, 3)) == List(2, 3))
		println(tail(Nil) == Nil)
		println(tail(List(1, 2)) == List(2))
	}

}
