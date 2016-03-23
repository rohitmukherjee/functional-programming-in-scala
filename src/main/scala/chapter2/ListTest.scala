package main.scala.chapter2

object ListTest {
	def tailTest = {
		assert(List.tail(List(1, 2, 3)) == List(2, 3))
		assert(List.tail(Nil) == Nil)
		assert(List.tail(List(1, 2)) == List(2))
	}

}