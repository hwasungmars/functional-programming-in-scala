package info.technicallycorrect.fpscala

/**
 * Option trait.
 */
sealed trait Optional[+A] {
  def map[B](f: A => B): Optional[B] = this match {
    case Empty => Empty
    case Something(value) => Something(f(value))
  }

  def flatMap[B](f: A => Optional[B]): Optional[B] = this match {
    case Empty => Empty
    case Something(value) => f(value)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Empty => default
    case Something(value) => value
  }
  def orElse[B >: A](ob: => Optional[B]): Optional[B] = this match {
    case Empty => ob
    case Something(value) => this
  }

  def filter(f: A => Boolean): Optional[A] = this match {
    case Empty => Empty
    case Something(value) => if (f(value)) this else Empty
  }
}

case class Something[+A](get: A) extends Optional[A]
case object Empty extends Optional[Nothing]
