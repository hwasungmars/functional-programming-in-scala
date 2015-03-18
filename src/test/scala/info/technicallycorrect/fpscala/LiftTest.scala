package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}

class LiftTest extends FlatSpec with Matchers {

  "map2" should "lift a function of two optional variables" in {
    val fn: (Int, Int) => Int = (x, y) => x + y
    Lift.map2(None, Some(2))(fn) should be(None)
    Lift.map2(Some(2), None)(fn) should be(None)
    Lift.map2(Some(2), Some(3))(fn) should be(Some(5))
  }

  "sequence" should "return a None if any of the elements are None" in {
    Lift.sequence(List(Some(2), None, Some(3))) should be(None)
    Lift.sequence(List(Some(2), Some(3), Some(5))) should be(Some(List(2, 3, 5)))
  }

}
