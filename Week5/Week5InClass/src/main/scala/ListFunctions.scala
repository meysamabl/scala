object testListFuncs {

  def main(args: Array[String]): Unit = {
    val fruit = List("apples", "oranges", "pears", "bananas")
    val fruit2 = List("bananas")
    val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
    val empty = List()
    val ints = List(22, -2, 11, 3, 2, 5, 7, 88)
    val listFuncs = new ListFunctions
    println(listFuncs.length(empty))
    println(listFuncs.init(diag3))
    println(listFuncs.concat(fruit, fruit2))
    println(listFuncs.reverse(diag3))
    println(listFuncs.removeAt(0, diag3))
    println(listFuncs.mergeSort(fruit)(Ordering.String))
    // since the parameter is implicit, the compiler will synthesize for me
    println(listFuncs.mergeSort(fruit))
    println(listFuncs.pack(List("a", "a", "a", "b", "c", "c", "a")))
    println(listFuncs.encode(List("a", "a", "a", "b", "c", "c", "a")))
    println(listFuncs.foldConcat(fruit, fruit2))
    println(listFuncs.mapFun[Int, Int](ints, (x => x + 1)))
    println(listFuncs.lengthFun(fruit))

    println(ints filter (x => x > 0))
    println(ints filterNot (x => x > 0))
    println(ints partition (x => x > 0))

    println(ints takeWhile (x => x > 0))
    println(ints dropWhile (x => x > 0))
    println(ints span (x => x > 0))
  }
}

import scala.math.Ordering

class ListFunctions {

  def length[T](xs: List[T]): Int = {
    xs match {
      case List() => 0
      case y :: ys => 1 + length(ys)
    }
  }

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new NoSuchElementException("empty list")
    case y :: ys => if (ys.isEmpty) y else last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](x: Int, xs: List[T]): List[T] = xs.take(x) ::: xs.drop(x + 1)

  def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(mergeSort(fst), mergeSort(snd))
    }

  }

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs).map(list => (list.head, list.length))

  def foldConcat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_ :: _)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) ((x, y) => f(x) :: y)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ((x, y) => y + 1)
}
