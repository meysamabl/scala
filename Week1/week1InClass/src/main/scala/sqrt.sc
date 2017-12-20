def sqrt(x: Double) = {
  def abs(x:Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x)/x < 1e-06

  def improve(guess: Double) =(guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(2)
sqrt(9)
sqrt(1e-6)
sqrt(1e60)



