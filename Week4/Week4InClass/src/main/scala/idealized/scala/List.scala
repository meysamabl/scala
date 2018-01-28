package idealized.scala

object ListMain {
  def main(args: Array[String]): Unit = {
    println(List(8, 9))
    println(List(3,4,5))
    println(List().prepend(3))
  }
}

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)

}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Head Nil")

  def tail: Nothing = throw new NoSuchElementException("Tail Nil")

  override def toString: String = "."
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false

  override def toString: String = "{" + head + "}" + "-> " + tail
}

object List {
  // List(1,2) = List.apply(1,2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))

  def apply[T](x1: T, x2: T, x3: T): List[T] = new Cons(x1, new Cons(x2, new Cons(x3, Nil)))

  def apply[T](): List[T] = Nil
}
