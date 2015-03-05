package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}

class FoldTest extends FlatSpec with Matchers {

  "foldLeft" should "tail recursively loop and fold" in {
    val list = List(1, 2, 3, 4, 5)
    Fold.foldLeft(list, 0)(_ + _) should be(15)
  }

  "sum" should "sum up the elements" in {
    Fold.sum(List(1, 2, 3)) should be(6)
  }

  "product" should "multiply the elements" in {
    Fold.product(List(1, 2, 3, 4)) should be(24)
  }

  "length" should "return the length of the list" in {
    Fold.length(List(1, 2)) should be(2)
  }

  "reverse" should "reverse the list" in {
    Fold.reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  "foldRight" should "fold from right" in {
    Fold.foldRight(List(3, 2, 1), 0)(_ - _) should be(2)
  }

}
