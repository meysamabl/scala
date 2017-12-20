def factTail(x: Double) : Double = {
  def factAux(acc: Double, x: Double) :Double =
    if (x == 0) acc else factAux(x*acc, x-1)
  factAux(1, x)
}




factTail(5)
factTail(4)