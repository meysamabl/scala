println((1 to 5) flatMap (x => (1 to 5) map (y => (x, y))))

println(scalarProduct(Vector(1, 2, 3), Vector(2, 2, 2)))
println(isPrime(2))
println(isPrime(13))
println(isPrime(22))
println(isPrime(15485863))
println(isSumPrime(7))
println(isSumPrimeForExprs(7))

def scalarProduct(v1: Vector[Double], v2: Vector[Double]): Double =
  (for ((x, y) <- v1.zip(v2)) yield x * y).sum
// different implementation of the scalarProduct
// (v1.zip(v2)).map(xy => xy._1 * xy._2).sum
// (v1.zip(v2)).map { case (x, y) => x * y }.sum
/* (v1.zip(v2)).map(x => x match {
     case (x, y) => x * y
   }).sum
*/

def isPrime(n: Int): Boolean = (2 until n).forall(x => n % x != 0)

def isSumPrime(n: Int): Seq[(Int, Int)] =
  (1 until n) flatMap (x => (1 until x).map(y => (x, y))) filter (pair => isPrime(pair._1 + pair._2))

def isSumPrimeForExprs(n: Int): Seq[(Int, Int)] =
  for {
    i <- 1 until n
    j <- 1 until i
    if (isPrime(i + j))
  } yield (i, j)

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- (0 until n)
        if (isSafe(col, queens))
      } yield col :: queens

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRows = (row - 1 to 0 by -1) zip queens
    queensWithRows forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  placeQueens(n)
}

def show(queens: List[Int]) = {
  val lines = for {
    col <- queens.reverse
  } yield Vector.fill(queens.length)("* ")
    .updated(col, "X ") mkString

  "\n" + (lines mkString("\n"))
}

queens(4) map show