class Poly(terms0: Map[Int, Double]) {

  val terms = terms0 withDefaultValue 0.0
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  //def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
//    terms get exp match {
//      case Some(coeff1) => exp -> (coeff + coeff1)
//      case  None => exp -> coeff
//    }
  }

  def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerms))
  def addTerms(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString: String =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"X^"+exp) mkString " + "
}

val poly1 = new Poly(1 -> 2, 3 -> -3, 4 -> 1)
var poly2 = new Poly(1 -> 3.0, 3 -> 4.0, 0 -> 5.0)

poly1 + poly2
