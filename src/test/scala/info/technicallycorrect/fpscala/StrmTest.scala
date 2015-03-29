package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}


class StrmTest extends FlatSpec with Matchers {

  "toList" should "return a list of evaluated Strm" in {
    val as: Strm[Int] = Strm.cons(1, Strm.cons(2, Strm.empty))
    as.toList should be(List(1, 2))
  }

  "take" should "return the first n elements of a Strm" in {
    val as: Strm[Int] = Strm.cons(1, Strm.cons(2, Strm.cons(3, Strm.empty)))
    as.take(2).toList should be(Strm.cons(1, Strm.cons(2, Strm.empty)).toList)
  }

  "drop" should "drop the first n elements" in {
    val as: Strm[Int] = Strm.cons(1, Strm.cons(2, Strm.cons(3, Strm.empty)))
    as.drop(1).toList should be(Strm.cons(2, Strm.cons(3, Strm.empty)).toList)
  }

  "foldRight" should "fold elements from right" in {
    val as: Strm[Int] = Strm.cons(1, Strm.cons(2, Strm.cons(3, Strm.empty)))
    def fn(x: Int, y: => Int) = x + y
    as.foldRight(0)(fn) should be(6)
  }

  "takeWhile" should "take elements while the condition is true" in {
    val as: Strm[Int] = Strm.cons(1, Strm.cons(2, Strm.cons(3, Strm.empty)))
    as.takeWhile(_ % 2 == 0).toList should be(List(2))
  }

  "forAll" should "checks whether all elements statisfy the condition" in {
    val as: Strm[Int] = Strm.cons(1, Strm.cons(2, Strm.cons(3, Strm.empty)))
    as.forAll(_ > 0) should be(true)
    as.forAll(_ % 2 == 0) should be(false)
  }

  "headOption" should "return a Option value on head" in {
    val as: Strm[Int] = Strm.cons(1, Strm.cons(2, Strm.cons(3, Strm.empty)))
    as.headOption should be(Some(1))
    EmptyStrm.headOption should be(None)
  }

  "map" should "lazily map" in {
    val as: Strm[Int] = Strm.cons(1, Strm.cons(2, Strm.cons(3, Strm.empty)))
    as.map(_ + 2).toList should be(Strm.apply(3, 4, 5).toList)
  }

  "filter" should "lazily filter" in {
    val as: Strm[Int] = Strm.apply(1, 2, 3)
    as.filter(_ % 2 == 0).toList should be(Strm.apply(2).toList)
  }

  "append" should "append to the head" in {
    val as: Strm[Int] = Strm.apply(1, 2, 3)
    as.append(0).toList should be(Strm.apply(0, 1, 2, 3).toList)
  }

}
