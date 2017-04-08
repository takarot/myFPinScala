package GettingStarted

object MyModule {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial value of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = 
      if(n<=0) acc
      else go(n-1, n*acc)
    
    go(n, 1)
  }
    
  def fib(n: Int): Int ={
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int = 
      if(n==0) prev
      else go(n-1, cur, prev+cur) 
    go(n, 0, 1)
  }

// not general purpose function
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    
    loop(0)
  }
 
// general purpose function

  def findFirst2[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if( p( as(n) ) ) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if(n >= as.length-1) true
      else if(ordered(as(n), as(n+1))) false
      else loop(n+1)
    }

    loop(0)
  }

  def main(args: Array[String]): Unit ={
    println(formatAbs(-122))
    println(formatFactorial(7))
    println(findFirst2(Array(7, 9, 13), (x: Int) => x == 9))
  }
}
