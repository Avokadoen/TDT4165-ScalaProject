import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def withdraw(amount: Double): Either[Double, RuntimeException] = balance.synchronized {
        if(balance.amount < amount) return Right(new NoSufficientFundsException())
        if(amount <= 0) return Right(new IllegalAmountException())
        balance.amount -= amount
        Left(amount)
    }

    def deposit (amount: Double): Either[Double, RuntimeException] = balance.synchronized {
        if(amount <= 0) return Right(new IllegalAmountException())
        balance.amount += amount
        Left(amount)
    }

    def getBalanceAmount: Double = balance.synchronized(balance.amount)

    def transferTo(account: Account, amount: Double): Unit = {
        bank addTransactionToQueue (this, account, amount)
    }


}
