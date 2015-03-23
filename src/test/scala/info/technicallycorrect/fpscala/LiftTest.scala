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

  "traverse" should "return None if f returns None" in {
    val evenNone: Int => Option[Int] = (x: Int) => if (x % 2 == 0) None else Some(x)
    Lift.traverse(List(2, 3, 5))(evenNone) should be(None)
    Lift.traverse(List(3, 5, 7))(evenNone) should be(Some(List(3, 5, 7)))
  }

  "sequence2" should "behave as sequence but implemented with traverse" in {
    Lift.sequence2(List(Some(2), None, Some(3))) should be(None)
    Lift.sequence2(List(Some(2), Some(3), Some(5))) should be(Some(List(2, 3, 5)))
  }

}
