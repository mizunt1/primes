//** A sequential implementation of the Sieve of Eratosthenes */
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
      var next = 2 // next candidate prime to consider
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
        }
        next += 1
      }

      // About 2.7 secs for N = 1,000,000; answer: 15,485,863
    }

    def concurrent(num_primes:Int) = {
      //print("start concurrent")
      val num_threads = 4
      val primes = new AtomicIntegerArray(num_primes)
      primes.set(0,2)
      val working_on = new AtomicIntegerArray(num_threads)
      //initalise this array with all Nones
      var l = 0
      while(l<num_threads){working_on.set(l, -1); l+=1}
      // ie.e thread one is working on 1, thread 2 is working on 3
      val primes_filled = new AtomicInteger(1)
      val next_working = new AtomicInteger(2)
      // next is the next prime to consider
      // nextSlot is the next free index in the array of primes
      // println("start work0")
      def worker(){
        //println("start work")
        while(primes_filled.get() < num_primes){
          val next = next_working.getAndIncrement()
          val next_prime = next + 1
          working_on.set((Thread.currentThread().getId().toInt%num_threads), next_prime)

          //while there is a number in working_on where number**2 is smaller
          //then next, it will remain in this loop 
          // HURDLE ONE ///
          // checking that we can check is this number is prime
          // spin until that smaller prime is no longer in working on.
          var j = 0;
          while(j< num_threads){
            val working_on_testing = working_on.get(j)
            while(working_on_testing != -1 && (working_on_testing * working_on_testing.toLong) <= next_prime){}
            j += 1
          }
          // value no longer in working on array

          // HURDLE TWO check whether next_working is a prime.
          var i = 0;
          var p = primes.get(i);
          // p!=-0 added for the case when for example, primes=[2,5,0,0], a possible configuration of primes array
          // three has not yet been added yet
          while(p != 0 && (p.toLong*p <=next_prime && next_prime%p != 0)){i += 1; p = primes.get(i)}

          if (p.toLong*p>next_prime || p == 0){
            // HURDLE THREE
            // fill the primes array CORRECTLY
            var k = 0
            var saved = next_prime
            // k will scan through the primes array to shuffle numbers
            // This while loop fills the array
            while(k < num_primes){
              var old_value = primes.get(k)
              var success = false
              if(old_value==0){success = primes.compareAndSet(k, old_value, saved);
                if(success){k+= num_primes}}
              else if(old_value < next_prime){}
              else if(old_value > next_prime){
                success = primes.compareAndSet(k, old_value, saved);
                if(success){saved=old_value}
              }
              else {}
            k += 1
            }
            primes_filled.getAndIncrement()
            working_on.set((Thread.currentThread().getId().toInt%num_threads), -1)
            
          }
        }
        var j = 0

      }

      ox.cads.util.ThreadUtil.runSystem(num_threads, worker)
      println("num_primes th prime" + primes.get(num_primes-1))
      println("Time taken: "+(java.lang.System.currentTimeMillis()-t0))
    }
       
    concurrent(N)



  }
}
