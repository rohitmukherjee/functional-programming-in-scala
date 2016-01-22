package chapter1

object PolymorphicFunctions {
	/**
	 * Monomorphic functions -> defined only for one type of data
	 * Polymorphic functions -> defined for any type -> Generics or container types
	 */
	def binarySearch(ds: Array[Double], key: Double): Int = {
		@annotation.tailrec
		def go(min: Int, mid: Int, max: Int): Int = {
			if (min > max)
				-1
			else if (ds(mid) == key)
				mid
			else if (ds(mid) > key)
				go(min, (mid - 1 + min) / 2, mid - 1)
			else
				go(mid + 1, (mid + 1 + max) / 2, max)
		}
		go(0, (ds.length - 1) / 2, ds.length - 1)
	}

	def main(args: Array[String]): Unit = {
		println(binarySearch(Array(1, 2, 3, 4, 5), 5))
		println(binarySearch(Array(0, 1, 8, 13, 14, 15), 1))
		println(binarySearch(Array(1, 2, 3, 4, 5), 6))
		/**
		 * This type of polymorphism is different from the definition used in
		 * OOP which has more to do with sub - types. This type of polymorphism is
		 * called parametric polymorphism.
		 */
		println(binarySearch(Array(1, 2, 3, 4, 5), 5, (x: Int, y: Int) => x > y))
		println(isSorted(Array(1, 2, 5, 7, 10), (x: Int, y: Int) => x > y))
		println(isSorted(Array(1, 4, 5, 7, 10), (x: Int, y: Int) => x > y))
		println(isSorted(Array(1, 1, 1, 7, 10), (x: Int, y: Int) => x > y))
		println(isSorted(Array(1, 1, 8, 7, 10), (x: Int, y: Int) => x > y))
	}

	def binarySearch[A](ds: Array[A], key: A, gt: (A, A) => Boolean): Int = {
		@annotation.tailrec
		def go(min: Int, mid: Int, max: Int): Int = {
			if (min > max)
				-1
			else if (ds(mid) == key)
				mid
			else if (gt(ds(mid), key))
				go(min, (mid - 1 + min) / 2, mid - 1)
			else
				go(mid + 1, (mid + 1 + max) / 2, max)
		}
		go(0, (ds.length - 1) / 2, ds.length - 1)
	}

	def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
		@annotation.tailrec
		def go(index1: Int, index2: Int): Boolean = {
			if (index1 == as.length - 1)
				true
			else if (!gt(as(index1), as(index2)))
				go(index2, index2 + 1)
			else false
		}
		go(0, 1)
	}
}