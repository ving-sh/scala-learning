def isPrime(n: Int): Boolean = {
  if (n <= 1) false
  else if (n == 2) true
  else if (n % 2 == 0) false
  else {
    // Check for divisibility from 3 up to the square root of n, only odd divisors
    val limit = Math.sqrt(n).toInt
    !(3 to limit by 2).exists(divisor => n % divisor == 0)
  }
}


(1 to 100000).map(isPrime)