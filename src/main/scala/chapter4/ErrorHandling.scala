package chapter4

import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException

object ErrorHandling {

	// Exercise 2
	def variance(xs: Seq[Double]): Option[Double] = {
		mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
	}

	def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.size)

	def pattern(s: String): Option[Pattern] =
		try {
			Some(Pattern.compile(s))
		} catch {
			case e: PatternSyntaxException => None
		}

	def mkMatcher(pat: String): Option[String => Boolean] =
		pattern(pat) map (p => (s: String) => p.matcher(s).matches)

	def mkMatcher_1(pat: String): Option[String => Boolean] =
		for {
			p <- pattern(pat)
		} yield ((s: String) => p.matcher(s).matches)

	def doesMatch(pat: String, s: String): Option[Boolean] =
		for {
			p <- mkMatcher_1(pat)
		} yield p(s)

	/// Exercise 3
	def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		for {
			aValue <- a
			bValue <- b
		} yield {
			f(aValue, bValue)
		}
	}

	// Exercise 3 alternative without sugar
	def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		a flatMap (aValue =>
			b map (bValue =>
				f(aValue, bValue)))
	}

	def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] = {
		for {
			f <- mkMatcher(pat)
			g <- mkMatcher(pat2)
		} yield f(s) && g(s)
	}

	// Exercise 4
	def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
		map2(mkMatcher(pat1), mkMatcher(pat2))(???)
	}

	// Exercise 5
	def sequence[A](a: List[Option[A]]): Option[List[A]] = {
		Some(a.filterNot(_ == None).map(_.get))
	}

	def parsePatterns(a: List[String]): Option[List[Pattern]] =
		sequence(a map pattern)

	// Exercise 6
	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
		sequence(a map f)
	}

	def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
		if (xs.isEmpty) Left("mean of empty list is not defined")
		else Right(xs.sum / xs.size)
	}

	def safeDivision(x: Double, y: Double): Either[Exception, Double] = {
		try {
			Right(x / y)
		} catch {
			case e: Exception => Left(e)
		}
	}

	// TODO: Exercise 8
	def sequence[EE, A](a: List[Either[EE, A]]): Either[EE, List[A]] = {
		???
	}

	def traverse[EE, A, B](a: List[A])(f: A => Either[EE, B]): Either[EE, List[B]] = {
		sequence(a map f)
	}

	case class Person(name: Name, age: Age)
	sealed class Name(val value: String)
	sealed class Age(val value: Int)

	def mkName(name: String): Either[String, Name] = {
		if (name == "" || name == null) Left("Name is empty.")
		else Right(new Name(name))
	}

	def mkAge(age: Int): Either[String, Age] = {
		if (age < 0) Left("Age is out of range.")
		else Right(new Age(age))
	}

	def mkPerson(name: String, age: Int): Either[String, Person] = {
		mkName(name).map2(mkAge(age))(Person(_, _))
	}

}