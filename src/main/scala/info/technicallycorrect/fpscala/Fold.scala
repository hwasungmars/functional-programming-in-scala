package info.technicallycorrect.fpscala

/**
 * Fold related questions in Chapter 2.
 */
object Fold {

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(lst: List[A], rslt: B): B = lst match {
      case Nil => rslt
      case l :: ls => loop(ls, f(rslt, l))
    }

    loop(as, z)
  }

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

}
