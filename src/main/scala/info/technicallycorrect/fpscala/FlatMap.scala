package info.technicallycorrect.fpscala


object FlatMap {

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.length == 0) None else Some(xs.sum / xs.length)
  }

}
