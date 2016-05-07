package recfun

object Main {

    def main(args: Array[String]) {
      println("Pascal's Triangle")
      for (row <- 0 to 10) {
          for (col <- 0 to row)
              print(pascal(col, row) + " ")
          println()
      }

      println( balance(":-)" toList ) )
      println(countChange(4, List(1,2)))
    }


    /**
     * Exercise 1
     */
    def pascal(c: Int, r: Int):Int = if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)


    /**
    * Exercise 2
    */
    def balance(chars: List[Char]): Boolean = {
      if (chars.isEmpty || !chars.contains('(')) return false
      for(c <- chars if c == '(' ) {
        val tail = chars.tail
        if (!tail.contains(')')) return false
        return if (tail.contains('(')) balance(tail) else true
      }
      false
    }

    /**
    * Exercise 3
    */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money <= 0 || coins == null || coins.isEmpty) return 0

      val coinsType = coins.sorted.distinct

      def getCoin(byType: Int): Int = {
        if (byType <= 0 || byType > coinsType.size) return 0
        coinsType apply byType-1
      }

      def cc(amount: Int, coinTypes: List[Int]): Int = {
        if (amount == 0) return 1
        if (amount <  0 || coinTypes.isEmpty) return 0
        cc (amount, coinTypes.dropRight(1)) + cc(amount - getCoin(coinTypes.size), coinTypes)
      }

      cc(money, coins)
    }
}