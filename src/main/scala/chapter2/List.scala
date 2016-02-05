package main.scala.chapter2

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(integers: List[Int]): Int = integers match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}
}