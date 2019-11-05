/** A sequential implementation of the Sieve of Eratosthenes */

object Sieve{

  def main(args: Array[String]) = {
    assert(args.length == 1, "must have one argument")


    val N = args(0).toInt // number of primes required
    // array will hold values with type Int and will be len N
    val primes = new Array[Int](N) // will hold the primes
    // first prime on the list is 2
    primes(0) = 2
    var nextSlot = 1 // next free slot in primes
    var next = 3 // next candidate prime to consider
    
    def sequential(nextSlotIn:Int) = {
      val t0 = java.lang.System.currentTimeMillis()
      print(t0)
      var nextSlot = nextSlotIn
      while(nextSlot<N)
      {
        // Test if next is prime;
        // invariant: next is coprime with primes[0..i) && p = primes(i)
        var i = 0; var p = primes(i)
        // while loop to go through primes list and see if next is prime.
        // p is basically going through all the list of primes weve already accumilated
        // while next is equal to, or larger than p**2, then choose the next larger prime
        // if

        while(p*p<=next && next%p != 0){ i += 1; p = primes(i) }
        if(p*p>next)
        { // next is prime
          // add element to primes array and start again at var i = 0, primes (i) = 2
          // but this time with a new value of next incremented by 1
          primes(nextSlot) = next; nextSlot += 1
          // println(next)
        }
        next += 1
      }
      // print the last prime:
      println(primes(N-1))
      print(java.lang.System.currentTimeMillis())
      println("Time taken: "+(java.lang.System.currentTimeMillis()-t0))
      // About 2.7 secs for N = 1,000,000; answer: 15,485,863
    }
    sequential(nextSlot)
  }
}
