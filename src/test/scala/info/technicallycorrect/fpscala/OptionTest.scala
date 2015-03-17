package info.technicallycorrect.fpscala

import org.scalatest.{Matchers, FlatSpec}

class OptionTest extends FlatSpec with Matchers {

  "map" should "map the element" in {
    val fn: Int => Int = x => x + 2
    Something(4).map(fn) should be(Something(6))
    Empty.map(fn) should be(Empty)
  }

  "flatMap" should "flatMap the element" in {
    val fn: Int => Optional[Int] = x => Something(x + 2)
    Something(4).flatMap(fn) should be(Something(6))
    Empty.flatMap(fn) should be(Empty)
  }

  "getOrElse" should "get the value or return default" in {
    val value: Int = 2
    Something(4).getOrElse(value) should be(4)
    Empty.getOrElse(value) should be(2)
  }

  "orElse" should "return the Optional value" in {
    val value: Optional[Int] = Something(2)
    Something(4).orElse(value) should be(Something(4))
    Empty.getOrElse(value) should be(Something(2))
  }

  "filter" should "filter the value" in {
    val fn: Int => Boolean = x => x % 2 == 0
    Something(4).filter(fn) should be(Something(4))
    Something(3).filter(fn) should be(Empty)
    Empty.filter(fn) should be(Empty)
  }

}
