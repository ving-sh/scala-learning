import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

object ActorBankAccount extends App {
  case class Withdraw(amount: Int)

  class BankAccountActor extends Actor {
    private var balance: Int = 1000

    def receive: Receive = {
      case Withdraw(amount) =>
        // No need for explicit synchronization - actor processes one message at a time
        if (balance >= amount) {
          // Simulate some processing time
          Thread.sleep(100)
          balance -= amount
          println(s"Withdrew $amount, remaining balance: $balance")
        } else {
          println(s"Insufficient funds: $balance")
        }
    }
  }


  // Create the actor system
  val system = ActorSystem("BankSystem")

  // Create the bank account actor
  val account = system.actorOf(Props[BankAccountActor](), "account")

  account ! Withdraw(1000)
  account ! Withdraw(300)

  Thread.sleep(1000)
  system.terminate()
}