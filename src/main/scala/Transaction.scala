import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  private val queue = mutable.Queue[Transaction]()

  // Remove and return the first element from the queue
  def pop: Transaction = queue.synchronized {
    queue.dequeue
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = queue.synchronized {
    queue.isEmpty
  }

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = queue.synchronized {
    queue.enqueue(t)
  }

  // Return the first element from the queue without removing it
  def peek: Transaction = queue.synchronized {
    queue.front
  }

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = queue.synchronized {
    queue.iterator
  }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttempts: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  var attempt = 0

  override def run(): Unit = {

    def doTransaction(): Unit = this.synchronized {
      from withdraw amount match {
        case Left(a) =>
          // Deposit can't fail here since withdraw can not return a negative value
          to deposit a
          status = TransactionStatus.SUCCESS
        case Right(_) =>
          attempt += 1
          if (attempt >= allowedAttempts) status = TransactionStatus.FAILED
      }
    }

    if (status == TransactionStatus.PENDING && attempt < allowedAttempts) {
      doTransaction()
      Thread.sleep(50)   // you might want this to make more room for
    }                    // new transactions to be added to the queue
    else status = TransactionStatus.FAILED
  }
}
