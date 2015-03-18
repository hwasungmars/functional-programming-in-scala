package info.technicallycorrect.fpscala

/**
 * Lifting functions to Option space.
 */
object Lift {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
    case None => None
    case Some(avalue) => b match {
      case None => None
      case Some(bvalue) => Some(f(avalue, bvalue))
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def fn(a: Option[A], result: Option[List[A]]): Option[List[A]] = (a, result) match {
      case (Some(x), Some(xs)) => Some(x :: xs)
      case _ => None
    }

    a.foldRight(Option(List[A]()))(fn)
  }


}
