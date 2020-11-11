object task2 extends App {
  // Task 2: a)
  def makeThread(f: => Unit): Thread = {
    new Thread {
      override def run() = f
    }
  }

  // b)
  private var counter: Int = 0

  def increaseCounter(): Unit = {
    counter += 1
  }

  def printCounter(): Unit = {
    println(counter)
  }

  makeThread(increaseCounter()).start
  makeThread(increaseCounter()).start
  makeThread(printCounter()).start

  // c)
  def betterIncreaseCounter(): Unit = counter.synchronized {
    counter += 1
  }

  // d)

  object A {
    lazy val a: Int = B.b
    lazy val a1: Int = 1
  }

  object B {
    lazy val b:Int = A.a1
  }

  /*
  this.synchronized {
    val thread1 = makeThread(A.a)
    val thread2 = makeThread(B.b)

    thread1.start
    thread2.start
  }
  */

  lazy val x: Int = {
    val thread = makeThread(println(s"initializing $x"))
    thread.start
    thread.join
    1
  }
  x

}
