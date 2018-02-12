package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      val disc = delta()
      lazy val negativeB = -1 * b()
      lazy val doubleA = 2.0 * a()
      lazy val rootDelta = Math.sqrt(disc)
      lazy val solution1 = (negativeB + rootDelta) / doubleA
      lazy val solution2 = (negativeB - rootDelta) / doubleA
      if(disc < 0) Set()
      else if (solution1 == solution2) Set(solution1)
      else Set(solution1, solution2)
    })
  }
}
