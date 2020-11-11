import task1_2.{ScalaIntroduction, Task2}

import scala.annotation.tailrec

object task1 extends App {
    def forLoopArray(): Array[Int] = {
        val arrCount = 50
        val arr = new Array[Int](arrCount)
        for (i <- arr.indices) {
            arr(i) = i + 1
        }

        arr
    }

    def sumArrayFor(arr: Array[Int]): Int = {
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

    // There is a lot difference between BigInt and Int.
    // Int is a **primitive** 32 bit signed integer. As long as you are working
    // with numbers between -2147483648 to 2147483647, then you should use Int.
    // BigInt is actually a **class** as you can find the definition of BigInt:
    // scala/package.scala:
    // type BigInt = scala.math.BigInt
    // BigInt.Scala:
    // final class BigInt(val bigInteger: BigInteger)
    // It allows for extremely big numbers at the cost of performance by combining
    // memory locations to form one big number in contrast with Int which only takes
    // one memory location. So BigInt is a integer with x bit size and is software implemented
    // Int is mostly defined in the spec of most modern CPU architecture.
    def fibo(n: Int): BigInt = {
        if (n <= 1) {
            return n
        }

        fibo(n - 1) + fibo(n - 2)
    }
}

// Class used for testing. Included for transparancy, and if you want to hack it to run
// this code originally had the structure of normal scala/java code where
// src
// | - main/scala/task1
// | - test/scala/task1
class ScalaIntroductionTests extends FunSuite {
    val scalaIntroduction = new ScalaIntroduction
    val arr: Array[Int] = scalaIntroduction.forLoopArray()

    test("Test task 1 a: for loop that assigns array") {
        arr.length shouldBe 50
        arr(0) shouldBe 1
        arr(49) shouldBe 50
    }

    test("Test task 1 b: sum of elements in array with for") {
        val sum = scalaIntroduction.sumArrayFor(arr)
        sum shouldBe 1275
    }

    test("Test task 1 c: sum of elements in array with recursion") {
        val sum = scalaIntroduction.sumArrayRecurs(arr)
        sum shouldBe 1275
    }

    test("Test task 1 d: create a fibonacci function without optimizations") {
        scalaIntroduction.fibo(0) shouldBe 0
        scalaIntroduction.fibo(1) shouldBe 1
        scalaIntroduction.fibo(2) shouldBe 1
        scalaIntroduction.fibo(3) shouldBe 2
        scalaIntroduction.fibo(8) shouldBe 21
        scalaIntroduction.fibo(20) shouldBe 6765
    }

    test("Test task 2 a: create thread with simple counter") {
        var counter = 0

        def count(): Unit = {
            for (_ <- 1 to 1000)
                counter += 1
        }

        val t1 = new Task2().createThread(count)
        t1.start()
        t1.join()
        counter shouldBe 1000
    }
}
