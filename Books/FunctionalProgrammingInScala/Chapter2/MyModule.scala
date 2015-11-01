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

    private def formatFactorial (n : Int) = {
      val msg = "The factorial of %d is %d"
      msg.format(n, factorial(n))
    }

    def formatResult (name: String, n: Int, f: Int => Int): String = {
      val msg = "The %s of %d is %d"
      msg.format(name, n, f(n))
    }

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      @annotation.tailrec
      def loop(a: Int, b: Int, fun: (A,A) => Boolean) : Boolean = {
        if (b >= as.length) true
        else if (fun(as(a),as(b)) == false) false
        else loop (b, b + 1, fun)
      }
      loop (0,1,ordered)
    }

    def compose[A,B,C](f: B => C, g: A => B): A => C = {
        a => f(g(a))
    }

    def main (args: Array[String]): Unit = {
      println(formatAbs(-42))
    }
}
