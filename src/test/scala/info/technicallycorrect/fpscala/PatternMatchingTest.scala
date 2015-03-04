package info.technicallycorrect.fpscala

import org.scalatest.{Matchers, FlatSpec}

class PatternMatchingTest extends FlatSpec with Matchers {

  "tail" should "return the tail of a list" in {
    PatternMatching.tail(List(1, 2, 3)) should be (List(2, 3))
    PatternMatching.tail(Nil) should be (Nil)
  }

}
