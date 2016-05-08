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
		var acc = initialValue
		var these = l
		while (!isEmpty(these)) {
			acc = f(acc, head(these))
			these = tail(these)
		}
		acc
	}

	private def isEmpty[A](l: List[A]): Boolean = l match {
		case Nil => true
		case _ => false
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

	// Exercise 13
	def foldLeftUsingFoldRight[A, B](integers: List[A], initialValue: B)(f: (A, B) => B): B = {
		val reversedList = reverse(integers)
		foldRight(reversedList, initialValue)(f)
	}

	// Exercise 13
	def foldRightUsingFoldLeft[A](integers: List[A]): List[A] = {
		// TODO:foldRightUsingFoldLeft 
		???
	}

	// Exercise 14
	def appendUsingFoldLeft[A](first: List[A], second: List[A]): List[A] = {
		// TODO:appendUsingFoldLeft
		???
	}

	def appendUsingFoldRight[A](first: List[A], second: List[A]): List[A] = {
		// TODO:appendUsingFoldRight
		???
	}

	// Exercise 16
	def addOne(integers: List[Int]): List[Int] = {
		reverse(foldRight(integers, Nil: List[Int])((x, y) => Cons(x + 1, y)))
	}

	// Exercise 17
	def doubleToString(integers: List[Double]): List[String] = {
		reverse(foldRight(integers, Nil: List[String])((x, y) => Cons(x.toString, y)))
	}

	// Exercise 18
	def map[A, B](l: List[A])(f: A => B): List[B] = {
		reverse(foldRight(l, Nil: List[B])((x, y) => Cons(f(x), y)))
	}

	def mapWithoutFold[A, B](l: List[A])(f: A => B): List[B] = {
		l match {
			case Nil => Nil
			case Cons(head, tail) => Cons(f(head), map(tail)(f))
		}
	}

	// Exercise 19
	def filter[A](l: List[A])(f: A => Boolean): List[A] = {
		reverse(foldRight(l, Nil: List[A])((x, y) => {
			if (f(x)) {
				Cons(x, y)
			} else {
				y
			}
		}))
	}

	// Exercise 20
	def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
		l match {
			case Nil => Nil
			case Cons(head, tail) => append(f(head), flatMap(tail)(f))
		}
	}

	// Exercise 21
	def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
		flatMap(l)(x => if (f(x)) List(x) else Nil)
	}

	// Exercise 22
	def addLists(l1: List[Int], l2: List[Int]): List[Int] = {
		require(length(l1) == length(l2), "Both lists should be of equal length")
		def inner(cl1: List[Int], cl2: List[Int], sumList: List[Int]): List[Int] = {
			(cl1, cl2) match {
				case (Nil, Nil) => sumList
				case (Cons(h1, t1), Cons(h2, t2)) => inner(t1, t2, Cons(h1 + h2, sumList))
			}
		}
		reverse(inner(l1, l2, Nil: List[Int]))
	}

	// Exercise 23
	def addListsGeneric[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
		require(length(l1) == length(l2), "Both lists should be of equal length")
		def inner(cl1: List[A], cl2: List[A], sumList: List[A]): List[A] = {
			(cl1, cl2) match {
				case (Nil, Nil) => sumList
				case (Cons(h1, t1), Cons(h2, t2)) => inner(t1, t2, Cons(f(h1, h2), sumList))
			}
		}
		reverse(inner(l1, l2, Nil: List[A]))
	}

	// Exercise 24 
	def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
		// TODO: Fix this function
		def inner(l1: List[A], sub1: List[A], started: Boolean): Boolean = {
			if (sub != Nil && l1 == Nil) {
				false
			} else if (sub == Nil) {
				true
			} else if (head(l1) != head(sub1) && !started) {
				inner(tail(l1), sub, started)
			} else if (head(l1) != head(sub1) && started) {
				false
			} else {
				inner(tail(l1), tail(sub), true)
			}
		}
		inner(l, sub, false)
	}
}
