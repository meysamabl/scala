package idealized.scala

object BooleanMain extends App {
  val meysam: IBoolean = fals
  println(meysam <= tru)
}

object tru extends IBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = t

  override def toString: String = "true"
}

object fals extends IBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = e

  override def toString: String = "false"
}

abstract class IBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  def &&(x: IBoolean): IBoolean = ifThenElse(x, fals)

  def ||(x: IBoolean): IBoolean = ifThenElse(tru, x)

  def unary_! : IBoolean = ifThenElse(fals, tru)

  def ==(x: IBoolean): IBoolean = ifThenElse(x, x.unary_!)

  def !=(x: IBoolean): IBoolean = ifThenElse(x.unary_!, x)

  def <(x: IBoolean): IBoolean = ifThenElse(fals, x) // assume fals < tru
  def <=(x: IBoolean): IBoolean = ifThenElse(x, tru)

  def >(x: IBoolean): IBoolean = ifThenElse(x.unary_!, fals)

  def >=(x: IBoolean): IBoolean = ifThenElse(tru, x.unary_!)

}