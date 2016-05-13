package chapter4

sealed trait Option[+A] {

	// Exercise 1
	def map[B](f: A => B): Option[B] = {
		this match {
			case Some(value) => Some(f(value))
			case _ => None
		}
	}

	def flatMap[B](f: A => Option[B]): Option[B] = {
		this.map(f) getOrElse None
	}

	// B is a supertype of A
	def getOrElse[B >: A](default: => B): B = {
		this match {
			case Some(value) => value
			case None => default
		}
	}

	def get: A = {
		this match {
			case None => throw new RuntimeException("None.get is not defined")
			case Some(value) => value
		}
	}

	def orElse[B >: A](default: => Option[B]): Option[B] = {
		this match {
			case None => default
			case Some(value) => Some(value)
		}
	}

	def filter(f: A => Boolean): Option[A] = {
		this match {
			case Some(value) => { if (f(value)) Some(value) else None }
			case None => None
		}
	}

	/*
	 * Option composition and lifting: 
	 */
	def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]