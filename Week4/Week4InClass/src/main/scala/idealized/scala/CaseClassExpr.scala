package idealized.scala

trait Expr1 {
  def eval: Int = this match {
    case Number1(x) => x
    case Sum1(x,y) => x.eval + y.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  def show: String = {
    def paren(e: Expr1) : String = e match {
      case Sum1(_, _) => "(" + e.show + ")"
      case  (_) => e.show
    }

    this match {
      case Number1(x) => x.toString
      case Sum1(e1, e2) => e1.show + " + " + e2.show
      case Prod(e1, e2) => paren(e1) + " * " + paren(e2)
    }
  }
}
case class Number1(n: Int) extends Expr1
case class Sum1(e1: Expr1, e2: Expr1) extends Expr1
case class Prod(e1: Expr1, e2: Expr1) extends Expr1
case class Variable(name: String) extends Expr1


object testMain extends App {

  // eval standalone function
  def eval(e: Expr1): Int = e match {
    case Number1(x) => x
    case Sum1(e1,e2) => eval(e1) + eval(e2)
  }

  val e = Sum1(Number1(11), Number1(2))
  // object-oriented option
  println(e.eval)
  println(Sum1(Number1(11), Prod(Number1(2), Number1(2))).eval)
  println(Prod(Number1(11), Sum1(Prod(Sum1(Number1(3), Number1(4)), Number1(2)), Number1(2))).show)
  // standalone option
  println(eval(e))

  println(e.show)

}
