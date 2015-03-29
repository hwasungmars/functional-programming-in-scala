package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}

class LazyTest extends FlatSpec with Matchers {

  "from" should "lazily return a infinite stream" in {
    val moreThan3 = Lazy.from(3)
    moreThan3.take(3) should be(Stream(3, 4, 5))
  }

  "fibs" should "lazily return a fibonacci stream" in {
    Lazy.fibs(0).take(3) should be(Stream(0, 1, 1))
  }

  "unfold" should "be a able to do fibs, from ones" in {
    val fibs = (x: List[Int]) => Some((x.sum, List(x.last, x.sum)))
    Lazy.unfold(List(-1, 1))(fibs).take(3) should be(Stream(0, 1, 1))

    val from = (x: Int) => Some((x, x + 1))
    Lazy.unfold(3)(from).take(2) should be(Stream(3, 4))

    val ones = (x: Int) => Some((1, 1))
    Lazy.unfold(1)(ones).take(2) should be(Stream(1, 1))
  }

  "map" should "map via unfold" in {
    Lazy.map((x: Int) => x + 2)(Stream(1, 2, 3)).toList should be(List(3, 4, 5))
  }

  "take" should "take via unfold" in {
    Lazy.take(2)(Stream(1, 2, 3)) should be(Stream(1, 2))
  }

}
