package idealized.scala

object ExprMain extends App {
  def eval(e: Expr): Int =
    if(e.isNumber) e.numberVal
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error("Not known Expression" + e)

  val test = eval(new Sum(new Number(91), new Number(2)))
  println(test)
}

trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def numberVal: Int
  def leftOp: Expr
  def rightOp: Expr
}

class Number(val numberVal: Int) extends Expr {
  def isNumber: Boolean = true

  def isSum: Boolean = false

  def leftOp: Expr = throw new Error("number.leftOp")

  def rightOp: Expr = throw new Error("number.rightOp")
}

class Sum(val leftOp: Expr, val rightOp: Expr) extends Expr {
  def isNumber: Boolean = false

  def isSum: Boolean = true

  def numberVal: Int = throw new Error("sum.numberVal")
}