package info.technicallycorrect.fpscala

/**
 * Map related exercises in Chapter 2.
 */
object Map {

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    Fold.foldRight(as, List[A]())((e, r) => if (f(e)) e :: r else r)
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    Map.flatMap(as)(x => if (f(x)) List(x) else List[A]())
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    Fold.concat(Map.map(as)(f))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    Fold.foldRight(as, List[B]())((e, r) => f(e) :: r)
  }

}
