package info.technicallycorrect.fpscala

/**
 * Either class implementation.
 */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(value) => Left(value)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case Left(value) => Left(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => Right(value)
    case Left(value) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(value), Right(bvalue)) => Right(f(value, bvalue))
    case (Left(value), _) => Left(value)
    case (_, Left(value)) => Left(value)
  }

  def sequence[EE >: E, AA](es: List[Either[EE, AA]]): Either[EE, List[AA]] = {
    this match {
      case Right(value) => {
        def loop(a: Either[EE, AA], result: Either[EE, List[AA]]): Either[EE, List[AA]] =
          (a, result)
          match {
            case (Right(avalue), Right(rvalue)) => Right(avalue :: rvalue)
            case (Left(avalue), _) => Left(avalue)
            case (_, Left(rvalue)) => Left(rvalue)
          }

        val z: Either[EE, List[AA]] = Right(List[AA]())
        es.foldRight(z)(loop)
      }
      case Left(value) => Left(value)
    }
  }

  def traverse[EE >: E, AA, B](as: List[AA])(f: AA => Either[EE, B]): Either[EE, List[B]] =
    this match {
      case Right(value) => {
        def loop(a: AA, result: Either[EE, List[B]]): Either[EE, List[B]] = (f(a), result) match {
          case (Right(avalue), Right(rvalue)) => Right(avalue :: rvalue)
          case (_, Left(rvalue)) => Left(rvalue)
          case (Left(avalue), _) => Left(avalue)
        }

        val z: Either[EE, List[B]] = Right(List[B]())
        as.foldRight(z)(loop)
      }
      case Left(value) => Left(value)
    }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

