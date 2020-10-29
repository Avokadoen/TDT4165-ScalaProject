object task1 extends App {
  // Task 1: a)
  var arr: Array[Int] = Array()
  for (i <- 1 to 50) arr :+= i
  for (i <- arr) println(i)

  var arr1 = new Array[Int](50)
  for (i <- 1 to 50) arr1(i - 1) = i
  for (i <- arr1) println(i)

  // b)
  def sum(arr: Array[Int]): Int = {
    var sum = 0
    for (i <- arr) sum += i
    sum
  }

  println(sum(arr))

  // c)
  def sumRec(arr: Array[Int]): Int = {
    if (arr.length > 0) arr.head + sumRec(arr.tail)
    else 0
  }

  println(sumRec(arr))

  // d)
  def fib(n: BigInt): BigInt = {
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)
  }

  for (i <- 0 to 10) println(fib(i))
}
