package info.technicallycorrect.fpscala

/**
 * Pattern matching from Chapter 2.
 */
object PatternMatching {

  def tail[A](xs: List[A]): List[A] = xs match {
    case _ :: tail => tail
    case Nil => Nil
  }

}
