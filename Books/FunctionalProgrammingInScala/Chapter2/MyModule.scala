object MyModule {
    def abs (n : Int): Int = {
      if (n < 0) -n
      else n
    }

    def factorial (n: Int): Int = {
      @annotation.tailrec
      def go(n: Int, acc: Int): Int =
        if (n <= 0) acc
        else go (n - 1, n * acc)

      go(n, 1)
    }

    def fib(n: Int): Int = {
      @annotation.tailrec
      def go(n: Int, a: Int, b: Int): Int =
        if (n == 0) a
        else go(n - 1, b, a+b)

      go(n, 0, 1)
    }

    private def formatAbs (x : Int) = {
      val msg = "The absolute value of %d is %d"
      msg.format(x, abs(x))
    }

    def main (args: Array[String]): Unit = {
      println(formatAbs(-42))
    }
}
