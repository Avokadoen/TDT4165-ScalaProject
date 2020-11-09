import scala.annotation.tailrec

class Bank(val allowedAttempts: Integer = 3) {

    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()

    /**
     * @param from   the account we are withdrawing money from
     * @param to     the account that recieves withdrawn money
     * @param amount the amount we want to transfer
     */
    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        // create a transaction
        val transaction = new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
        transactionsQueue.push(transaction)

        // process the transaction in another thread
        Main.thread(processTransactions)
    }

    /**
     * Process the first transaction in the queue, if it completes it will be put in the 'processedTransactions'
     * queue, otherwise we push it in the 'transactionsQueue' and attempts to process it further
     */
    private def processTransactions(): Unit = {
        this.synchronized {
            val transaction = transactionsQueue.pop
            val thread = Main.thread(transaction.run)
            thread.join
            if (transaction.status == TransactionStatus.PENDING) {
                transactionsQueue.push(transaction)
                processTransactions
            }
            else processedTransactions.push(transaction)
        }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
