package task1_2

object RunTask1And2 {
  def run(): Unit = {
    // Since task 2 is about printing, we use the runtime instead of tests to
    // see this. Also we use a for loop to avoid having to run the program multiple times
    for (_ <- 0 to 10) {
      println("\nRunning loop, task 2b then task2c")

      //        val task2b = new Task2()
      //        task2b.runTask2B()

      //        val task2c = new Task2()
      //        task2c.runTask2C()
    }

    // Task 2 D
    new Thread(() => new Task2().createSimpleDeadLock()).start()

    // Do it 10 times, will *probably* meet deadlock with that many attempts
    //    for (_ <- 0 to 10) {
    //        new Thread(() => new Task2().createPossibleLazyDeadlock()).start()
    //    }

    println("done")
  }
}
