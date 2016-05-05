package main.scala.chapter2

object ListTest {
	def tailTest = {
		assert(List.tail(List(1, 2, 3)) == List(2, 3))
		assert(List.tail(Nil) == Nil)
		assert(List.tail(List(1, 2)) == List(2))
	}

	def reverseTest = {
		assert(List.reverse(Nil) == Nil)
		assert(List.reverse(List(1)) == List(1))
		assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
	}

	def appendTest = {
		assert(List.append(List(1, 2, 3), List(1, 2)) == List(1, 2, 3, 1, 2))
		assert(List.append(List(1), List(1, 2)) == List(1, 1, 2))
		assert(List.append(Nil, List(1, 2)) == List(1, 2))
	}

	def foldLeftTest = {
		assert(List.foldLeft(List(5, 4, 1), 10)(_ - _) == 0)
	}

	def foldRightTest = {
		assert(List.foldRight(List(10, 4, 1), 10)(_ - _) == -3)
	}

	def addOneTest = {
		assert(List.addOne(List(1, 2, 3)) == List(2, 3, 4))
		assert(List.addOne(Nil) == Nil)
		assert(List.addOne(List(1)) == List(2))
	}

	def doubleToStringTest = {
		assert(List.doubleToString(List(1.0)) == List("1.0"))
		assert(List.doubleToString(Nil) == Nil)
		assert(List.doubleToString(List(1, 2.1, 3.2, 4.4)) == List("1.0", "2.1", "3.2", "4.4"))
	}

}