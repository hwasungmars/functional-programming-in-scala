package info.technicallycorrect.fpscala

/**
 * Pattern matching from Chapter 2.
 */
object PatternMatching {

  def tail[A](xs: List[A]): List[A] = xs match {
    case _ :: tail => tail
    case Nil => Nil
  }

  def setHead[A](x: A, xs: List[A]): List[A] = xs match {
    case _ :: tail => x :: tail
    case Nil => List(x)
  }

  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case _ :: tail => if (n > 0) drop(tail, n - 1) else xs
    case Nil => Nil
  }

  def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = lst match {
    case l :: ls => if (f(l)) l :: dropWhile(ls, f) else dropWhile(ls, f)
    case Nil => Nil
  }

  def init[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case l :: Nil => Nil
    case l :: ls => l :: init(ls)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop(big: List[A], small: List[A]): Boolean = {
      if (big.length < small.length) false
      else if (big.take(small.length) == small) true
      else loop(big.tail, small)
    }

    loop(sup, sub)
  }

}
