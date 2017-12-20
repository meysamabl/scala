def gcd(a: Double, b: Double) : Double =
  if (b == 0) a else gcd(b, a % b)

gcd(21,14)
gcd(48, 12)
gcd(28,21)
gcd(15,35)
