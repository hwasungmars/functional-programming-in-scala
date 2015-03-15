package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}

class MapTest extends FlatSpec with Matchers {

  "map" should "behave like map" in {
    Map.map(List(1, 2, 3))(_ + 1) should be(List(2, 3, 4))
  }

  "filter" should "behave like filter" in {
    Map.filter(List(1, 2, 3))(_ % 2 == 0) should be(List(2))
  }

  "flatMap" should "be like a Monad" in {
    Map.flatMap(List(1, 2, 3))(List(_)) should be(List(1, 2, 3))
  }

  "filterViaFlatMap" should "behave like filter" in {
    Map.filterViaFlatMap(List(1, 2, 3))(_ % 2 == 0) should be(List(2))
  }

  "zipWith" should "behave like zip map" in {
    Map.zipWith(List(1, 2, 3), List(6, 5, 4))(_ + _) should be(List(7, 7, 7))
    Map.zipWith(List(1, 2, 3), List(6, 5))(_ + _) should be(List(7, 7))
  }

}
