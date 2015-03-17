package info.technicallycorrect.fpscala

import org.scalatest.{Matchers, FlatSpec}


class FlatMapTest extends FlatSpec with Matchers {

  "variance" should "compute the variance" in {
    FlatMap.variance(List(-1, 0, 1)) should be(Some(2.0/3))
    FlatMap.variance(List()) should be(None)
  }

}
