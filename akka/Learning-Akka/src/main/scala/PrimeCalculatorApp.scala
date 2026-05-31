import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import akka.routing.RoundRobinPool

import java.time.{Duration, LocalDateTime}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*

// --- Messages for Actor Communication ---

case class CalculatePrimesInRange(start: Int, end: Int)
case class CheckNumber(number: Int)
case class IsPrimeResult(number: Int, isPrime: Boolean)
case class FinalPrimesList(primes: List[Int])


// --- Utility for Primality Testing ---
object PrimeUtil {

  def isPrime(n: Int): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else if (n % 2 == 0) false
    else {
      // Check for divisibility from 3 up to the square root of n, only odd divisors
      val limit = Math.sqrt(n).toInt
      Thread.sleep(5)
      !(3 to limit by 2).exists(divisor => n % divisor == 0)
    }
  }
}


class PrimeWorker extends Actor {
  override def receive: Receive = {
    case CheckNumber(number) =>
      val isNumPrime = PrimeUtil.isPrime(number)
      sender() ! IsPrimeResult(number, isNumPrime)
  }
}


class PrimeMaster(numWorkers: Int, listener: ActorRef) extends Actor {
  private val primesFound: ListBuffer[Int] = ListBuffer.empty[Int]

  private var totalNumbersToProcess: Int = 0
  private var resultsReceived: Int = 0

  private val workerRouter: ActorRef = context.actorOf(
    RoundRobinPool(numWorkers).props(Props[PrimeWorker]()),
    "workerRouter"
  )

  override def preStart(): Unit = {
    println(s"PrimeMaster starting with $numWorkers workers.")
  }

  override def receive: Receive = {
    case CalculatePrimesInRange(start, end) =>
      println(s"PrimeMaster: Received request to calculate primes from $start to $end.")
      if (start > end || start < 0) {
        println("PrimeMaster: Invalid range. Start must be non-negative and less than or equal to end.")
        listener ! FinalPrimesList(List.empty)
        context.stop(self)
      } else {
        primesFound.clear()
        totalNumbersToProcess = (start to end).count(n => true) // More robust way to count for large ranges if needed
        resultsReceived = 0

        if (totalNumbersToProcess == 0) { // Handle empty range like (5,4) or (1,0)
          println("PrimeMaster: Range is empty or invalid, no numbers to process.")
          checkIfAllDone()
        } else {
          // Distribute the work: send each number in the range to the workerRouter.
          // The router will then forward the CheckNumber message to one of the PrimeWorker actors.
          for (number <- start to end) {
            workerRouter ! CheckNumber(number)
          }
        }
      }

    case IsPrimeResult(number, true) =>
      primesFound += number
      resultsReceived += 1
      checkIfAllDone()

    case IsPrimeResult(_, false) =>
      resultsReceived += 1
      checkIfAllDone()

    case Terminated(actorRef) =>
      // This case can be used for more advanced supervision if a worker dies.
      // For this example, we're keeping it simple.
      println(s"PrimeMaster: Worker actor ${actorRef.path.name} terminated.")
  }


  private def checkIfAllDone(): Unit = {
    if (resultsReceived == totalNumbersToProcess) {
      println(s"PrimeMaster: All $totalNumbersToProcess numbers processed. Found ${primesFound.size} primes.")
      // Send the sorted list of primes to the listener actor.
      listener ! FinalPrimesList(primesFound.toList.sorted)
      // Stop the master actor itself. The router and its routees will also be stopped
      // because they are children of the master.
      println("PrimeMaster: Calculation complete. Stopping self.")
      context.stop(self)
    }
  }

  override def postStop(): Unit = {
    println("PrimeMaster stopped.")
  }
}


class PrimeAppListener(actorSystem: ActorSystem) extends Actor {
  override def preStart(): Unit = {
    println("PrimeAppListener started. Waiting for results...")
  }

  override def receive: Receive = {
    case FinalPrimesList(primes) =>
      println(s"\n--- Prime Calculation Complete ---")
      println(s"Total prime numbers found: ${primes.size}")
      if (primes.nonEmpty) {
        // For brevity, print only a limited number of primes if the list is very long.
        val primesToShow = 20
        println(s"First (up to) $primesToShow primes: ${primes.take(primesToShow).mkString(", ")}")
        if (primes.size > primesToShow) {
          println("...")
          println(s"Last (up to) $primesToShow primes: ${primes.takeRight(primesToShow).mkString(", ")}")
        }
      } else {
        println("No prime numbers found in the given range.")
      }
      println("----------------------------------")
      println("PrimeAppListener: Shutting down actor system...")
      actorSystem.terminate()
  }

  override def postStop(): Unit = {
    println("PrimeAppListener stopped.")
  }
}

// --- Main Application Object ---
object PrimeCalculatorApp extends App {
  println("Starting Prime Calculator Application...")
  val system = ActorSystem("PrimeCalculatorSystem")


  val rangeStart = 1
  val rangeEnd = 10000
  val numWorkers = 2 // Use number of available CPU cores for workers

  println(s"ActorSystem '${system.name}' created. Calculating primes from $rangeStart to $rangeEnd using $numWorkers workers.")

  val listener = system.actorOf(
    Props(new PrimeAppListener(system)), "primeListener")

  val master = system.actorOf(
    Props(new PrimeMaster(numWorkers, listener)), "primeMaster")

  val strtTime = LocalDateTime.now()
  master ! CalculatePrimesInRange(rangeStart, rangeEnd)


  system.whenTerminated.onComplete { _ =>
    println("ActorSystem has terminated. Exiting application.")
    println(s"It took ${Duration.between(strtTime, LocalDateTime.now()).toSeconds} seconds")
  }(system.dispatcher)

}
