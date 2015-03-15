package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}

class PatternMatchingTest extends FlatSpec with Matchers {

  "tail" should "return the tail of a list" in {
    PatternMatching.tail(List(1, 2, 3)) should be(List(2, 3))
    PatternMatching.tail(Nil) should be(Nil)
  }

  "setHead" should "return a new list with the head element" in {
    PatternMatching.setHead(4, List(1, 2, 3)) should be(List(4, 2, 3))
    PatternMatching.setHead(4, Nil) should be(List(4))
  }

  "drop" should "drop the first n elements" in {
    PatternMatching.drop(List(1, 2, 3), 0) should be(List(1, 2, 3))
    PatternMatching.drop(List(1, 2, 3), 2) should be(List(3))
    PatternMatching.drop(List(1, 2, 3), 3) should be(Nil)
    PatternMatching.drop(List(1, 2, 3), 4) should be(Nil)
  }

  "dropWhile" should "drop the elements if predicate is satisfied" in {
    val isOdd: Int => Boolean = x => x % 2 == 0
    PatternMatching.dropWhile(List(1, 2, 3), isOdd) should be(List(2))
  }

  "init" should "is Clojure's butlast" in {
    PatternMatching.init(Nil) should be(Nil)
    PatternMatching.init(List(1)) should be(Nil)
    PatternMatching.init(List(1, 2, 3)) should be(List(1, 2))
  }

  "hasSubsequence" should "is a linear scan" in {
    PatternMatching.hasSubsequence(List(1, 2, 3), List(2, 3)) should be(true)
    PatternMatching.hasSubsequence(List(1, 2, 3), List(3, 2)) should be(false)
  }

}
