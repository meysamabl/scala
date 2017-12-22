class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x,1)
  private def gcd(a: Int, b:Int): Int =
    if (b == 0) a else gcd(b, a%b)
  private val g = gcd(x,y)
  val numer = x / g
  val denum = y / g

  def + (that: Rational) : Rational =
    new Rational(numer * that.denum + that.numer * denum,
      denum * that.denum)

  def unary_- : Rational =
   new Rational(-numer, denum)

  def - (that: Rational) : Rational =
    this + -that

  def < (that: Rational) =
    numer* that.denum < that.numer * denum

  def max(that: Rational) =
    if(this < that) that else this

  override def toString: String =
    numer + "/" + denum
}


// if we want the class to be pure data structure
// then we add functions outside
def addRational(r1: Rational, r2: Rational): Rational =
  new Rational(r1.numer * r2.denum + r2.numer * r1.denum,
    r1.denum * r2.denum)

def makeString(r: Rational) =
  r.numer + "/" + r.denum

val r = addRational(new Rational(1,2), new Rational(2,3))
makeString(r)
r.denum

val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)
x - y - z
y + y
x max y

val test = new Rational(3)






