package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(
      Math.pow(b(), 2) - 4 * a() * c()
    )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val del = delta()
      if (del < 0)
        Set[Double]()
      else {
        val cb = b()
        val ca = a()
        Set((-cb + Math.sqrt(del))/(2*ca), (-cb-Math.sqrt(del))/(2*ca))
      }
    }
  }
}
