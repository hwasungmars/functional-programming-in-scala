package info.technicallycorrect.fpscala

/**
 * Fold related questions in Chapter 2.
 */
object Fold {

  def sum(list: List[Int]): Int = {
    foldLeft(list, 0)(_ + _)
  }

  def product(list: List[Int]): Int = {
    foldLeft(list, 1)(_ * _)
  }

  def length(list: List[Int]): Int = {
    foldLeft(list, 0)((x, y) => x + 1)
  }

  def reverse(list: List[Int]): List[Int] = {
    val fn: (List[Int], Int) => List[Int] = (r, x) => x :: r
    foldLeft(list, Nil: List[Int])(fn)
  }

  def append[A](as: List[A], a: A): List[A] = {
    foldRight(as, List(a))(_ :: _)
  }

  def concat[A](listOfLists: List[List[A]]): List[A] = {
    foldLeft(listOfLists, List[A]())(_ ++ _)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(lst: List[A], rslt: B): B = lst match {
      case Nil => rslt
      case l :: ls => loop(ls, f(rslt, l))
    }

    loop(as, z)
  }

  def inc(list: List[Int]): List[Int] = {
    foldRight(list, List[Int]())((e, r) => (e + 1) :: r)
  }

  def toString(list: List[Double]): List[String] = {
    foldRight(list, List[String]())(_.toString :: _)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    // We want to express f(a0, f(a1, ..., f(an, z))).  We cannot directly return a result of
    // type B because foldLeft runs from left to right.  We need to delay the evaluation and
    // build up a function instead.  Consider at the moment, i then we have g = f(a0, ..., f(ai, _))
    // We should get the next element and compose it g(f(a, _)).  That is the solution below.
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

}
