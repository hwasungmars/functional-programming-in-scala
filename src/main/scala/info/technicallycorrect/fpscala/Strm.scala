package info.technicallycorrect.fpscala

sealed trait Strm[+A] {

  def toList: List[A] = this match {
    case EmptyStrm => List[A]()
    case Cons(hd, tl) => hd() :: tl().toList
  }

  def take(n: Int): Strm[A] = this match {
    case EmptyStrm => EmptyStrm
    case Cons(hd, tl) => if (n <= 0) EmptyStrm else Strm.cons(hd(), tl().take(n - 1))
  }

  def drop(n: Int): Strm[A] = this match {
    case EmptyStrm => EmptyStrm
    case Cons(hd, tl) => if (n == 1) tl() else tl().drop(n - 1)
  }

  def foldRight[B](z: => B)(fn: (A, => B) => B): B = this match {
    case EmptyStrm => z
    case Cons(hd, tl) => fn(hd(), tl().foldRight(z)(fn))
  }

  def takeWhile(p: A => Boolean): Strm[A] = {
    def loop(a: A, rslt: => Strm[A]): Strm[A] = p(a) match {
      case true => Strm.cons(a, rslt)
      case false => rslt
    }

    this.foldRight(Strm.empty[A])(loop)
  }

  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true)((a, rslt) => p(a) && rslt)
  }

  def headOption(): Option[A] = this match {
    case EmptyStrm => None
    case Cons(hd, tl) => Some(hd())
  }

  def map[B](fn: A => B): Strm[B] = {
    def loop(a: A, rslt: => Strm[B]) = {
      Strm.cons(fn(a), rslt)
    }
    foldRight(Strm.empty[B])(loop)
  }

  def filter(fn: A => Boolean): Strm[A] = {
    def loop(a: A, rslt: => Strm[A]) = fn(a) match {
      case true => Strm.cons(a, rslt)
      case false => rslt
    }

    foldRight(Strm.empty[A])(loop)
  }

  def append[B >: A](x: B): Strm[B] = {
    Strm.cons(x, this)
  }

}

case object EmptyStrm extends Strm[Nothing]

case class Cons[+A](h: () => A, t: () => Strm[A]) extends Strm[A]

object Strm {

  def cons[A](hd: => A, tl: => Strm[A]): Strm[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Strm[A] = EmptyStrm

  def apply[A](as: A*): Strm[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}

