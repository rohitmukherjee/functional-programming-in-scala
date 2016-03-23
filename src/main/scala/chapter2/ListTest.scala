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

}