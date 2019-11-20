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
          // println(next)
        }
        next += 1
      }
      // print the last prime:
      //println(primes(N-1))
      //print(java.lang.System.currentTimeMillis())
      //println("Time taken: "+(java.lang.System.currentTimeMillis()-t0))
      // About 2.7 secs for N = 1,000,000; answer: 15,485,863
    }

    def concurrent(num_primes:Int) = {
      //print("start concurrent")
      val num_threads = 10
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
          //println("next working", next_prime)
          working_on.set((Thread.currentThread().getId().toInt%num_threads), next_prime)
          //println("%%%%%%%inwhile%%%%%%%%")
          //println("inwhile 1 primes filled", primes_filled.get())

          //while there is a number in working_on where number**2 is smaller
          //then next, it will remain in this loop 
          // HURDLE ONE ///
          // checking that we can check is this number is prime
          // spin until that smaller prime is no longer in working on.
          var j = 0;
          while(j< num_threads){
            while(working_on.get(j) != -1 && (working_on.get(j) * working_on.get(j)) <= next_prime){}
            //println("ok to work through this num", working_on.get(j),"for", next_prime)
            j += 1
          }
          // value no longer in working on array
          //println("out if loop for next working value",next_prime)
          //println(working_on.get(0),working_on.get(1), working_on.get(2))
          //println("next.get", next_prime)
          //println("state of primes", primes.get(0), primes.get(1), primes.get(2))

          // HURDLE TWO check whether next_working is a prime.
          var i = 0;
          var p = primes.get(i);
          // p!=-0 added for the case when for example, primes=[2,5,0,0], a possible configuration of primes array
          // three has not yet been added yet
          while(p != 0 && (p*p <=next_prime && next_prime%p != 0)){i += 1; p = primes.get(i)}

          if (p*p>next_prime || p == 0){
            //print("next working, this one is allowed to be appended", next_prime)
            // HURDLE THREE
            // fill the primes array CORRECTLY
            var k = 0
            var saved = next_prime
            //println("k before", k)
            //println("primesfilled before", primes_filled.get())
            // k will scan through the primes array to shuffle numbers
            // This while loop fills the array
            while(k < num_primes){
              //println("******inwhile4******")
              //print("what is k", k)
              //print("which prime are we fitting", saved)
              //println("state of primes while filling", primes.get(0), primes.get(1), primes.get(2))
              //print("primes filled", primes_filled.get())
              //print("num primes", num_primes)
              var old_value = primes.get(k)
              var success = false
              if(old_value==0){success = primes.compareAndSet(k, old_value, saved);
                if(success){k+= num_primes}}
              else if(primes.get(k) < next_prime){}
              else if(old_value > next_prime){
                success = primes.compareAndSet(k, old_value, saved);
                if(success){saved=old_value}
              }
              else {}
              //println("what is k ", k)

              //println("primes 0", primes.get(0))
              //println("primes_filled", primes_filled.get())
              //println("state of primes while filling", primes.get(0), primes.get(1), primes.get(2))
              // to save is the number currently not in the array which
              // will be placed in the array at the next iteration
              // when the prime is placed in the right place, increment k by extra one,
              // otherwise, just carry on
              //println("state of prime before incrementing k")
              var b = 0
              //while(b<num_primes){print(primes.get(b), " ");b+=1}
              k += 1
            }
            primes_filled.getAndIncrement()
            working_on.set((Thread.currentThread().getId().toInt%num_threads), -1)
            //println("working_on")
            var u = 0
            //while(u < num_threads){println(working_on.get(u)); u+=1}
            //println("state of primes after filling", primes.get(0), primes.get(1), primes.get(2))
          }
        }
        var j = 0
        //println("printing primes")
        //while(j < num_primes){println(primes.get(j)); j+=1}
        println("1000th prime", primes.get(num_primes-1))
        //println("end of primes")
      }
      ox.cads.util.ThreadUtil.runSystem(num_threads, worker)
    }
       
    concurrent(N)
  }
}
