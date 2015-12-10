package fpinscala.datastructures

sealed trait List[+A] // List datatype parameritized on type A
case object Nil extends List[Nothing] // A List constructor representing the empty List
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor

// List companion object. Contains functions for creating and working with Lists
object List {
  def sum (ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // variadic function syntax
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => Cons(h, Nil)
    case _ => Cons(h, l)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => if (n > 0) drop(xs, n-1) else l
  }

  // Exercise 3.5
  // test: List.dropWhile(l, (x:Int) => x < 4) // l = List(1,2,3,4,5,6)
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs, f) else l
  }

  // Exercise 3.6
  // test int(List(1,2,3,4))
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
  }



  // foldRight(List(1,2,3), 0)(f)
  // f(1, foldRight(List(2,3), 0) (f))
  // f(1, f(2, foldRight(List(3), 0) (f)))
  // f(1, f(2, f(3, foldRight(Nil, 0) (f))))
  // f(1, f(2, f(3, 0))) == f(1, f(2, 3 + 0))
  // f(1, f(2, 3)) == f(1, 2 + 3)
  // f(1, 5) == 1 + 5
  // 6

  // 1 + (foldRight(List(2,3), 0))
  // 1 + (2 + (foldRight(List(3), 0)))
  // 1 + (2 + (3 + (foldRight(Nil, 0))))
  // 1 + (2 + (3 + (0)))
  // 1 + (2 + (3 + 0))
  // 1 + (2 + 3)
  // 1 + 5

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  // foldLeft(List(4,5,6), 0)(+)
  // foldLeft(List(5,6), (0 + 4))
  // foldLeft(List(6), (5 + 4))
  // foldLeft(Nil, (6 + 9)) = 15

  // ((0 + 4) + 5) + 6 == 15
  // foldLeft(List(4,5,6), 0)(+)
  // foldLeft(List(5,6), (0 + 4))
  // foldLeft(List(6), ((0 + 4) + 5))
  // foldLeft(Nil, (((0 + 4) + 5) +6))
  // ((0 + 4) + 5) + 6
  // (4 + 5) + 6
  // 9 + 6 == 15


  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  // (_ * _) is concise for (x,y) => x * y
  def product2(ns: List[Int]) = foldRight(ns, 1)(_ * _)

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
}
