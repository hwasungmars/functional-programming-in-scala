package info.technicallycorrect.fpscala

object Lazy {

  def from(n: Int): Stream[Int] = {
    n #:: from(n + 1)
  }

  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }

  def fibs(n: Int): Stream[Int] = {
    fib(n) #:: fibs(n + 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => a #:: unfold(s)(f)
    case None => Stream[A]()
  }

  def map[A, B](f: A => B)(stream: Stream[A]): Stream[B] = {
    val mapper = (x: Stream[A]) => x match {
      case head #:: tail => Some(f(head), tail)
      case _ => None
    }
    unfold(stream)(mapper)
  }

  def take[A](n: Int)(stream: Stream[A]): Stream[A] = {
    def taker(s: (Int, Stream[A])): Option[(A, (Int, Stream[A]))] = s match {
      case (m, head #:: tail) => if (m > 0) Some(head, (m - 1, tail)) else None
      case _ => None
    }
    unfold((n, stream))(taker)
  }

}