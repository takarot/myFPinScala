package GettingStarted

/**
  * Created by takanori on 2016/09/05.
  */
object MyModule {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit ={
    println(formatAbs(-122))
  }
}