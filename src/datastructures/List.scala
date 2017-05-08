package myFPinScala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

// 引数にList[Double]型を受け取ってDouble型を返すメソッド.
// Listの要素をheadとtailに分けて,最後まで分けたら後ろから
// 処理が戻りつつ要素が掛けられる.
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

// 引数が可変長のメソッド.
// [A]型の引数を0個以上受け取る.
// 
  def apply[A](as: A*): List[A] = 
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail(x: List[Int]): List[Int] = x match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  def dropwhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropwhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
}

}

