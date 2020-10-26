package task1_2

import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

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

    val t1 = scalaIntroduction.createThread(count)
    t1.start()
    t1.join()
    counter shouldBe 1000
  }
}
