package task1_2

import scala.annotation.tailrec

class ScalaIntroduction {

  def forLoopArray(): Array[Int] = {
    val arrCount = 50
    val arr = new Array[Int](arrCount)
    for(i <- arr.indices) {
      arr(i) = i + 1
    }

    arr
  }

  def sumArrayFor(arr: Array[Int] ): Int = {
    var sum = 0
    for (member <- arr) {
      sum += member
    }

    sum
  }

  def sumArrayRecurs(arr: Array[Int]): Int = {
    // Ensure we have defined a tail recursive function
    @tailrec def go(arr: Array[Int], sum: Int): Int = {
      if (arr.length > 1) {
        // Note: I think slice here actually copies the whole thing for each call ...
        go(arr.slice(0, arr.length - 1), sum + arr.last)
      } else {
        sum + arr.last
      }
    }

    go(arr, 0)
  }

  // TODO: move this to the report
  // The difference between BigInt and Int is a lot.
  // Int is a **primitive** 32 bit signed integer. As long as you are working
  // with numbers between -2147483648 to 2147483647, then you should use Int.
  // BigInt is actually a **class** as you can find the definition of BigInt:
  // scala/package.scala:
  // type BigInt = scala.math.BigInt
  // BigInt.Scala:
  // final class BigInt(val bigInteger: BigInteger)
  // It allows for extremely big numbers at the cost of performance by combining
  // memory locations to form one big number in contrast with Int which only takes
  // one memory location
  def fibo(n: Int): BigInt = {
    if (n <= 1) {
      return n
    }

    fibo(n - 1) + fibo(n - 2)
  }
}
