package info.technicallycorrect.fpscala

/**
 * Random number generator from Chapter 6.
 */
trait RNG {
  def nextInt: (Int, RNG)

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

//  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rnga) = ra(rng)
    val (b, rngb) = rb(rng)
    (f(a, b), rnga)
  }
}

// A even simplified random number generator that actually increments the value each time.
case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}
