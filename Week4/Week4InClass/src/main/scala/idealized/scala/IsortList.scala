package idealized.scala


object IsortList {
  def insert(x: Int, xs: scala.List[Int]) : scala.List[Int] = {
    xs match {
      case scala.List() => scala.List(x)
      case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
    }
  }

  def isort(xs: scala.List[Int]): scala.List[Int] = xs match {
    case scala.List() => scala.List()
    case y::ys => insert(y, isort(ys))
  }

}

object testSort {
  def main(args: Array[String]): Unit = {
    val list = scala.List(11, 8, 5, 3, 10, 1)

    val sortedList = IsortList.isort(list)

    println(sortedList)
  }
}
