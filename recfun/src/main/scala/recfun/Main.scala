package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (r == 0 && c == 0)  1
      else if (c < 0 || c > r + 1 )  0
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
    * chars.isEmpty: Boolean returns whether a list is empty
    * chars.head: Char returns the first element of the list
    * chars.tail: List[Char] returns the list without the first element
   */
    def balance(chars: List[Char]): Boolean = {
      def newCount(c: Int, ch: Char): Int = {
        if (ch == '(') c + 1 else if (ch == ')') c - 1 else c
      }

      def balanceWithCount(chs: List[Char], count: Int): Boolean = {
        if (chs.isEmpty) count == 0
        else if (count < 0) false
        else
          balanceWithCount(chs.tail, newCount(count, chs.head))
      }

      balanceWithCount(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) return 0
      if (money == 0) return 1
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
