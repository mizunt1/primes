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
      print("start concurrent")
      val num_workers = 1
      val num_threads = 1
      val primes = new AtomicIntegerArray(num_primes)
      primes.set(0,2)
      val working_on = new AtomicIntegerArray(num_threads)
      // ie.e thread one is working on 1, thread 2 is working on 3
      val primes_filled = new AtomicInteger(1)
      val next = new AtomicInteger(3)
      // next is the next prime to consider
      // nextSlot is the next free index in the array of primes
      println("start work0")
      def worker(){
        println("start work")
        working_on.set((Thread.currentThread().getId().toInt%num_threads), next.get())
        while(primes_filled.get() < num_primes){
          println("inwhile")
          var i = 0;
          var j = 0;
          var p = primes.get(i);
          //while there is a number in working_on where number**2 is smaller
          //then next, it will remain in this loop 
          while(j < (num_threads - 1)){
            println("inwhile2")
            // reset counter j to zero if problem value is found.
            // counter will only reach num threads when all items in working on 
            // are non problem values.
            if(working_on.get(j) * working_on.get(j) < next.get()){j=0}
            println("in if")
            j += 1
            println("in if 2")
          }
          println("out if")
          println("next.get", next.get())
          println("primes .get i", primes.get(i))
          println("state of primes", primes.get(0), primes.get(1), primes.get(2))
          while(p*p <=next.get() && next.get()%p != 0){i += 1; println("what is next", next.get()); println("what is p", primes.get(i));println("what is i", i); p = primes.get(i)};
          println("inwhile3")
          var k = 1
          println("p**2", p*p)
          println("next get", next.get())
          if (p*p>next.get()){
            // fill the primes array
            var saved = next.get()
            while(k <= primes_filled.get()){
              println("******inwhile4******")
              print("what is k", k)
              print("what is state of primes", primes.get(9))
              if(k==num_primes){println("hello")}
              else if(primes.get(k) > next.get()|| primes.get(k)==0){saved = primes.getAndSet(k, saved)}
              println("what is k ", k)
              println("what is next", next.get())
              println("primes 0", primes.get(0))
              println("primes_filled", primes_filled.get())
              println("state of primes while filling", primes.get(0), primes.get(1), primes.get(2))
              // to save is the number currently not in the array which
              // will be placed in the array at the next iteration
              // when the prime is placed in the right place, increment k by extra one,
              // otherwise, just carry on
              println("primes after", primes.get(k))
              k += 1

            }
                          println("state of primes after filling", primes.get(0), primes.get(1), primes.get(2))
                        primes_filled.getAndIncrement()
          }
          next.getAndIncrement();

        }
        var j = 0
        while(j < num_primes){println(primes.get(j)); j+=1}
        
      }
      ox.cads.util.ThreadUtil.runSystem(num_workers, worker)
    }
       
    concurrent(N)
  //  var j = 0
//
  }
}
