class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    /**
     * @param amount the amount that will be taken from the account
     * @return Either the amount withdrawn from the account (left), or RuntimeException (right)
     */
    def withdraw(amount: Double): Either[Double, String] = this.synchronized {
        // if the amount in the account is insufficient
        if (balance.amount < amount) return Right("Insufficient funds")

        // if the withdraw amount is of illegal value
        if (amount < 0) return Right("Illegal amount")

        balance.amount -= amount
        Left(amount)
    }

    /**
     * @param amount the desired amount to store in this account
     * @return Either the amount we stored in the account (left), or a IllegalAmountException (right)
     */
    def deposit(amount: Double): Either[Double, String] = this.synchronized {
        // if the amount is of an illegal value
        if (amount < 0) return Right("Illegal amount")

        balance.amount += amount
        Left(amount)
    }

    def getBalanceAmount: Double = balance.synchronized(balance.amount)

    def transferTo(account: Account, amount: Double): Unit = {
        bank addTransactionToQueue(this, account, amount)
    }
}
