class Bank(val allowedAttempts: Integer = 3) {

    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()

    /**
     * @param from   the account we are withdrawing money from
     * @param to     the account that receives withdrawn money
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
    private def processTransactions(): Unit = this.synchronized {
        /*
        // SLOW af, but does not need sleep
        val transaction = transactionsQueue.pop
        val thread = Main.thread(transaction.run)
        thread.join     // does not fail with this, and does not need the sleep in Transaction
        // However just makes the thread obsolete anyway? Start thread, and wait for it to finish
        if (transaction.synchronized(transaction.status == TransactionStatus.PENDING)) {
            transactionsQueue.push(transaction)
            processTransactions
        }
        else processedTransactions.push(transaction)
         */

        // less slow, but needs sleep
        Main.thread {
            val transaction = transactionsQueue.pop
            transaction.run
            if (transaction.synchronized(transaction.status == TransactionStatus.PENDING)) {
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
