def fact(x: Double) : Double =
  if (x == 0) 1 else x * fact(x-1)

fact(5)
fact(4)
fact(10)