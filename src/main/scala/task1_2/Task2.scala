package task1_2

class Task2 {
  private var counter: Int = 0

  // Task 2 a
  def createThread(task: () => Unit): Thread = {
    new Thread(() => task())
  }

  // Task 2 b
  def increaseCounter(): Unit = {
    // Since it was hard to actually get a discrepancy, we use a for loop her to
    // increase the chance of the race condition occurring
    for (_ <- 0 to 100) {
      counter += 1
    }
  }

  // Task 2 c -> add 'this.synchronized' to introduce atomicity
  // the JVM will make sure to schedule this function between threads
  def atomIncreaseCounter(): Unit = this.synchronized {
    for (_ <- 0 to 100) {
      counter += 1
    }
  }

  def printCounter(): Unit = println(counter)

  // Utility function to create and start
  def createStart(task: () => Unit): Thread = {
    val t = createThread(task)
    t.start()
    t
  }

  // This phenomenon is called a "race condition". This happens when the program behaviour
  // depends on OS scheduling. Race conditions are a subset of something called "undefined behaviour"
  // meaning the programs behaviour is unpredictable. This is caused by doing things that are unpredictable
  // according to a given language specification.
  def runTask2B(): Unit = {
    val ict1  = createStart(increaseCounter)
    val ict2  = createStart(increaseCounter)
    val pct   = createStart(printCounter)

    ict1.join()
    ict2.join()
    pct.join()
  }

  def runTask2C(): Unit = {
    val ict1  = createStart(atomIncreaseCounter)
    val ict2  = createStart(atomIncreaseCounter)
    val pct   = createThread(printCounter) // we wait with starting the thread

    ict1.join()
    ict2.join()
    pct.start()
    pct.join()
  }

  // TASK 2 D
  // This function will create a deadlock if
  // - two threads has been assigned this function
  // - thread 1 is called with counter 1 as c1 and counter 2 as c2
  // - thread 2 is called with counter 2 as c1 and counter 1 as c2

  // SOURCE: https://en.wikipedia.org/wiki/Deadlock#Necessary_conditions
  // The reason the deadlock occurs is the following
  // - At least one mutex resource is currently being used (c1)
  // - Requesting additional resource which is being by the other thread (c2)
  // - The resource can be released only voluntarily by the process holding it (c1 & c2)
  // - Circular wait (thread 1 and thread 2 explained in description above)
  def createSimpleDeadLock(): Unit = {
    val counter1 = 0
    val counter2 = 2

    def workWork(c1: Int, c2: Int): Unit = {
      c1.synchronized {
        println(s"c1 is $c1")
        Thread.sleep(1000)

        c2.synchronized {
          println(s"c2 is $c2")
          Thread.sleep(1000)
        }
      }
    }

    new Thread(() => workWork(counter1, counter2)).start()
    new Thread(() => workWork(counter2, counter1)).start()

    // 1 second is more than enough for t1 and t2 theoretically,
    // but we will not see c2 print
    Thread.sleep(1000)
  }

  // Source: https://docs.scala-lang.org/sips/improved-lazy-val-initialization.html
  // pretty much the same as simple only using lazy evaluation
  object LazyDeadLock {
    lazy val l1: Int = 1
    lazy val l2: Int = 2
    lazy val l3: Int = {
     l2.synchronized {
        Thread.sleep(1000)
        l1.synchronized {
          l2 + l1
        }
      }
    }
    lazy val l4: Int = {
      l1.synchronized {
        Thread.sleep(1000)
        l2.synchronized {
          l2 + l1
        }
      }
    }
  }

  def createPossibleLazyDeadlock(): Unit = {
    def readLazy(l: Int): Unit = this.synchronized {
      println(l)
    }

    val t1 = new Thread(() => readLazy(LazyDeadLock.l3))
    val t2 = new Thread(() => readLazy(LazyDeadLock.l4))

    t1.start()
    t2.start()

    Thread.sleep(1000)
  }
}
