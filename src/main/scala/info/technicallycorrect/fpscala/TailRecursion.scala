package info.technicallycorrect.fpscala

/**
 * Tail recursion in Chapter 2 of Functional Programming in Scala.
 */
object TailRecursion {

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def loop(twoBefore: Int, oneBefore: Int, n: Int): Int = {
      if (n == 0) twoBefore + oneBefore
      else loop(oneBefore, oneBefore + twoBefore, n - 1)
    }

    loop(-1, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(xs: Array[A]): Boolean = {
      if (xs.length == 2 && ordered(xs(0), xs(1))) true
      else if (!ordered(xs(0), xs(1))) false
      else loop(xs.tail)
    }

    loop(as)
  }

}
