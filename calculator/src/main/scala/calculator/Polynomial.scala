package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val  A = a(); val B = b(); val C = c()
    B*B - 4*A*C
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val delta = computeDelta(a,b,c)
    val A = a()
    val B = b()
    delta() match {
      case x if x>0 => Set((B + Math.sqrt(x))/ (2*A),(-B - Math.sqrt(x))/ (2*A))
      case x if x == 0 => Set(-B/(2*A))
      case _ => Set()
    }
  }
}
