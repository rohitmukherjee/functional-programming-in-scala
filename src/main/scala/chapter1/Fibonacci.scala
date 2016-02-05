package chapter1

object Fibonacci {

	// Exercise 1
	def fib(n: Int) = {
		def inner(n1: Int, n2: Int, count: Int): Int = {
			if (count == 0)
				n1 + n2
			else
				inner(n2, n1 + n2, count - 1)
		}
		inner(0, 1, n)
	}

	def main(args: Array[String]) = {
		println(formatResult("factorial", 7, factorial))
		println(formatResult("fibonnaci", 7, fib))
		// Valid syntaxes for anonymous functions
		println(formatResult("increment", 7, (x: Int) => x + 1))
		println(formatResult("increment2", 7, (x) => x + 1))
		println(formatResult("increment3", 7, x => x + 1))
		println(formatResult("increment4", 7, _ + 1))
		println(formatResult("increment5", 7, x => { val r = x + 1; r }))
		// An anonymous function actually expands to the following class with an apply method
		val lessThan = new Function2[Int, Int, Boolean] {
			def apply(a: Int, b: Int) = a < b
		}
	}

	def factorial(n: Int): Int = {
		@annotation.tailrec
		def go(n: Int, acc: Int): Int = {
			if (n <= 0)
				acc
			else go(n - 1, acc * n)
		}
		go(n, 1)
	}

	// function taking (Int, String) and returning Int would be written as (Int, String) => Int
	def formatResult(name: String, n: Int, f: Int => Int): String = {
		val msg = "The %s of %d is %d"
		msg.format(name, n, f(n))
	}

}