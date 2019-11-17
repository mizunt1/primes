/** A sequential implementation of the Sieve of Eratosthenes */
import java.util.concurrent.atomic.AtomicInteger

import java.util.concurrent.atomic.AtomicIntegerArray

object Sieve{

  def main(args: Array[String]) = {
    assert(args.length == 1, "must have one argument")

    val t0 = java.lang.System.currentTimeMillis()
    val N = args(0).toInt // number of primes required
    // array will hold values with type Int and will be len N
    // first prime on the list is 2
    
    
    def sequential(num_primes:Int) = {
      val primes = new Array[Int](num_primes) // will hold the primes
      primes(0) = 2
      var next = 3 // next candidate prime to consider
      var nextSlot = 1 // next free slot in primes
      while(nextSlot<num_primes){
        // Test if next is prime;
        // invariant: next is coprime with primes[0..i) && p = primes(i)
        var i = 0; var p = primes(i)
        // while loop to go through primes list and see if next is prime.
        // p is basically going through all the list of primes weve already accumilated
        // while next is equal to, or larger than p**2, then choose the next larger prime
        // if

        while(p*p<=next && next%p != 0){ i += 1; p = primes(i) }
        if(p*p>next){
          // next is prime
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

    def concurrent(num_primes:Int) = {
      val num_workers = 1
      val num_threads = 1
      val primes = new AtomicIntegerArray(num_primes)
      val working_on = new AtomicIntegerArray(num_threads)
      // ie.e thread one is working on 1, thread 2 is working on 3
      val primes_filled = new AtomicInteger(1)
      val next = new AtomicInteger(3)
      val must_wait = True
      // next is the next prime to consider
      // nextSlot is the next free index in the array of primes
      def worker(){
        while(primes_filled.get() < num_primes){
          var i = 0;
          var j = 0;
          var p = primes.get(i);
          //while there is a number in working_on where number**2 is smaller
          //then next, it will remain in this loop 
          while(j < (num_threads - 1){
            // reset counter j to zero if problem value is found.
            // counter will only reach num threads when all items in working on 
            // are non problem values.
            if(working_on[j].get * working_on[j].get < next){j=0}
            j += 1
          }
          while(p*p <=next && next%p != 0){i += 1; p = primes.get(i)}
          if (p*p>next){
            // fill the primes array
            var to_save = primes[0].getAndUpdate(next);
            while(k< primes_filled){
              if(to_save > primes[k]){k+=1};
              to_save = primes[k].getAndUpdate(to_save);
              k += 1;
            }
            primes_filled.getAndIncrement()
          }
            next.getAndIncrement()
          }
        }}}
        }
sequential(N)
