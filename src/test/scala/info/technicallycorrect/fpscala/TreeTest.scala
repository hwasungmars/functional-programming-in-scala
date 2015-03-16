package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}


class TreeTest extends FlatSpec with Matchers {

  "size" should "return the size of the Tree" in {
    TreeUtils.size(Leaf(1)) should be(1)
    TreeUtils.size(Branch(Leaf(1), Leaf(2))) should be(3)
  }

  "max" should "return the maximum element of the Tree" in {
    TreeUtils.maximum(Leaf(2)) should be(2)
    TreeUtils.maximum(Branch(Leaf(1), Leaf(2))) should be(2)
  }

  "depth" should "return the depth of a Tree" in {
    TreeUtils.depth(Leaf(2)) should be(1)
    TreeUtils.depth(Branch(Leaf(1), Leaf(2))) should be(2)
    TreeUtils.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(3)
  }

  "map" should "map over the Tree" in {
    TreeUtils.map(Leaf(2))(_ * 2) should be(Leaf(4))
    TreeUtils.map(Branch(Leaf(1), Leaf(2)))(_ * 2) should be(Branch(Leaf(2), Leaf(4)))
  }

  "fold" should "fold over the Tree" in {
    TreeUtils.fold(Leaf(2))(0)(_ + _) should be(2)
    TreeUtils.fold(Branch(Leaf(1), Leaf(2)))(0)(_ + _) should be(3)
  }

}
