package info.technicallycorrect.fpscala

import org.scalatest.{Matchers, FlatSpec}

class TailRecursionTest extends FlatSpec with Matchers {

  "Fibonacci function" should "return base case of 0, 1, 1" in {
    TailRecursion.fibonacci(0) should be (0)
    TailRecursion.fibonacci(1) should be (1)
    TailRecursion.fibonacci(2) should be (1)
  }

  it should "also be able to return other cases" in {
    TailRecursion.fibonacci(3) should be (2)
    TailRecursion.fibonacci(5) should be (5)
  }

  "isSorted" should "report ordered arrays" in {
    val ordered = (x: Int, y: Int) => x < y
    TailRecursion.isSorted(Array(1, 2), ordered) should be (true)
    TailRecursion.isSorted(Array(1, 2, 3), ordered) should be (true)
    TailRecursion.isSorted(Array(2, 3, 1), ordered) should be (false)
  }

}
