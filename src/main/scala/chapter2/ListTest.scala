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

	def mapTest = {
		assert(List.map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
		assert(List.map(List(2, 3, 4))(_.toString) == List("2", "3", "4"))
		assert(List.map(Nil: List[Int])(_ + 1) == Nil)
	}

	def mapWithoutFoldTest = {
		assert(List.mapWithoutFold(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
		assert(List.mapWithoutFold(List(2, 3, 4))(_.toString) == List("2", "3", "4"))
		assert(List.mapWithoutFold(Nil: List[Int])(_ + 1) == Nil)
	}

	def filterTest = {
		assert(List.filter(List(1, 2, 3))(_ > 1) == List(2, 3))
		assert(List.filter(List(2, 3, 4))(_ % 2 == 0) == List(2, 4))
		assert(List.filter(Nil)(_ => true) == Nil)
	}

	def flatMapTest = {
		assert(List.flatMap(List(1, 2, 3))(s => List(s + 1)) == List(2, 3, 4))
		assert(List.flatMap(List(3, 4))(s => List(1 to s: _*)) == List(1, 2, 3, 1, 2, 3, 4))
		assert(List.flatMap(Nil: List[Int])(s => List(1 to s)) == Nil)
	}

	def filterUsingFlatMapTest = {
		assert(List.filterUsingFlatMap(List(1, 2, 3))(_ > 1) == List(2, 3))
		assert(List.filterUsingFlatMap(List(2, 3, 4))(_ % 2 == 0) == List(2, 4))
		assert(List.filterUsingFlatMap(Nil)(_ => true) == Nil)
	}

	def addListsTest = {
		assert(List.addLists(Nil, Nil) == Nil)
		assert(List.addLists(List(1, 2, 3), List(1, 2, 3)) == List(2, 4, 6))
		assert(List.addLists(List(1, 8), List(1, 2)) == List(2, 10))
	}

	def hasSubsequenceTest = {
		assert(List.hasSubsequence(List(1, 2, 3, 4), Nil) == true)
		assert(List.hasSubsequence(Nil, Nil) == true)
		assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3, 5)) == false)
		// TODO: Failing Test to fix
		//assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) == true)
	}

}