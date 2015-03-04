package info.technicallycorrect.fpscala

/**
 * Higher order functions in Chapter 2.
 */
object HigherOrderFunctions {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    x: A => (y: B) => f(x, y)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (x: A, y: B) => f(x)(y)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    x => f(g(x))
  }

}
