object task2 extends App {
  // Task 2: a)
  def gibThread(f: => Unit): Thread = {
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

  gibThread(increaseCounter()).start
  gibThread(increaseCounter()).start
  gibThread(printCounter()).start

  // c)
  def betterIncreaseCounter(): Unit = counter.synchronized {
    counter += 1
  }

  // d)
  // TODO make deadlock happen

}
