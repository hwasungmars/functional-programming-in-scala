package info.technicallycorrect.fpscala

import org.scalatest.{FlatSpec, Matchers}

class HigherOrderFunctionsTest extends FlatSpec with Matchers {

  "curry" should "return a curried function" in {
    val fn: (Int, Int) => Int = (x, y) => x + y
    HigherOrderFunctions.curry(fn)(2)(3) should be (5)
  }

  "uncurry" should "reverse currying" in {
    val fn: Int => Int => Int = x => y => x + y
    HigherOrderFunctions.uncurry(fn)(2, 3) should be (5)
  }

  "compose" should "compose two functions" in {
    val f: Int => String = _.toString
    val g: String => Int = _.toInt
    HigherOrderFunctions.compose(f, g)("2") should be ("2")
  }

}
