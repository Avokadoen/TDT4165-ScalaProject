import scala.annotation.tailrec

class Bank(val allowedAttempts: Integer = 3) {

    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        val t = new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
        transactionsQueue.push(t)
        val thread = new Thread(() => processTransactions())

        thread.start()
    }

    @tailrec
    private def processTransactions(): Unit = {
        val transaction = transactionsQueue.pop

        transaction.run()

        if (transaction.status == TransactionStatus.PENDING) {
            transactionsQueue.push(transaction)
            processTransactions()
        }
        else {
            processedTransactions.push(transaction)
        }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
