import scala.annotation.tailrec

class Bank(val allowedAttempts: Integer = 3) {

    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        val t = new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
        transactionsQueue.push(t)
        val thread = new Thread {
            override def run(): Unit = processTransactions()
        }
        thread.start()
    }

    @tailrec
    private def processTransactions(): Unit = {
        val t = transactionsQueue.pop
        /* val thread = new Thread(t)
        thread.start        // TODO Need to spawn thread here?? But it aint work :<
        thread.join */
        t.run()
        if (t.status == TransactionStatus.PENDING) {
            transactionsQueue.push(t)
            processTransactions()
        }
        else {
            processedTransactions.push(t)
        }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
