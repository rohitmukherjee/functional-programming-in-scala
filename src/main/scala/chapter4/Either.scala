package chapter4

/*
 * Options are sometimes inefficient when it comes to error handling and recovery. Option doesn't tell us much about
 * what happens in case of an exception happening. We might want to return a string/more info on what kind of exception
 * was raised. We can say that Either[+E, +A] is a disjoint union of two types.
 */

sealed trait Either[+E, +A] {

	// Exercise 7
	def map[B](f: A => B): Either[E, B] = {
		this match {
			case Left(value) => Left(value)
			case Right(value) => Right(f(value))
		}
	}

	def flatMap[EE >: E, A, B](f: A => Either[EE, B]): Either[EE, B] = {
		this match {
			case Left(value: EE) => Left(value)
			case Right(value: A) => f(value)
		}
	}

	def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
		this match {
			case Left(value) => b
			case Right(value) => Right(value)
		}
	}

	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
		???
	}

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
