package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(countChange(4, List(1,2,3)))
    println(countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if  (c == 0 || r == 0) 1
      else if (c == r) 1
      else pascal(c-1,r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceAux(chars : List[Char], acc: Int) : Int =
        if (chars.isEmpty || acc < 0) acc
        else if (chars.head == '(') balanceAux(chars.tail, acc+1)
        else if (chars.head == ')') balanceAux(chars.tail, acc-1)
        else  balanceAux(chars.tail, acc)

      balanceAux(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
      if (money < 0 || coins.isEmpty) 0 // is remaining amount is less than zero, or there is no coin and remaining amount is still not zero, no solution
      else if (money == 0) 1 // if the remaining amount is zero is recursive, there is solution
      // solution 1)
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
