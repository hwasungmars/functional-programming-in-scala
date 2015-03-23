package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {

  "map" should "map on the right" in {
    val fn = (x: Int) => x + 2
    Right(2).map(fn) should be(Right(4))
    Left("error").map(fn) should be(Left("error"))
  }

  "flatMap" should "flatMap on the right" in {
    val fn = (x: Int) => Right(x + 2)
    Right(2).flatMap(fn) should be(Right(4))
    Left("error").flatMap(fn) should be(Left("error"))
  }

  "orElse" should "return an Either" in {
    Right(2).orElse(Right(3)) should be(Right(2))
    Left(2).orElse(Right(3)) should be(Right(3))
  }

  "map2" should "take in a second parameter and map the function" in {
    val fn = (x: Int, y: Int) => x + y
    Right(2).map2(Right(3))(fn) should be(Right(5))
    Right(2).map2(Left("error"))(fn) should be(Left("error"))
    Left("error").map2(Right(3))(fn) should be(Left("error"))
    Left("error1").map2(Left("error2"))(fn) should be(Left("error1"))
  }

  "traverse" should "traverse a list with a function" in {
    val fn = (x: Int) => if (x % 2 != 0) Right(x + 2) else Left("even number!")
    Right(2).traverse(List(3, 5, 7))(fn) should be(Right(List(5, 7, 9)))
    Left("error").traverse(List(3, 5, 7))(fn) should be(Left("error"))
    Right(2).traverse(List(2, 3, 5))(fn) should be(Left("even number!"))
  }

  "sequence" should "return Either[E, List[A]] from List[Either[E, A]]" in {
    Right(2).sequence(List(Right(2), Right(3), Right(5))) should be(Right(List(2, 3, 5)))
    Left("error").sequence(List(Right(2), Right(3), Right(5))) should be(Left("error"))
    Right(2).sequence(List(Left("another"), Right(3), Right(5))) should be(Left("another"))  }

}
