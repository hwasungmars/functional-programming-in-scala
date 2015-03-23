package info.technicallycorrect.fpscala

/**
 * Lifting functions to Option space.
 */
object Lift {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def fn(a: Option[A], result: Option[List[A]]): Option[List[A]] = (a, result) match {
      case (Some(x), Some(xs)) => Some(x :: xs)
      case _ => None
    }

    a.foldRight(Option(List[A]()))(fn)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def fn(a: A, result: Option[List[B]]): Option[List[B]] = (f(a), result) match {
      case (Some(x), Some(xs)) => Some(x :: xs)
      case _ => None
    }

    a.foldRight(Option(List[B]()))(fn)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(identity[Option[A]])
  }

}
