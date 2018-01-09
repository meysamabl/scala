import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def nth[T](n: Int, list: List[T]) : T =
  if (n < 0 || list.isEmpty) throw new IndexOutOfBoundsException("n is less than zero")
  else if (n == 0) list.head
  else nth(n-1, list.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth[Int](2,list)