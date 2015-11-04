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
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => if (n > 0) drop(xs, n-1) else l
  }
}
