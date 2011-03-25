;  Calculation of Pi using quadrature. Parallel algorithm using pmap.
;
;  Copyright © 2009--2011 Russel Winder

;  TODO : Find out why the very long delay between completing the computation and terminating?

( defn partialSum [ parameter ]
  ( let [
         index  ( get parameter 0 )
         sliceSize  ( get parameter 1 )
         start  ( + 1 ( * index sliceSize ) )
         end  ( * ( + index 1 ) sliceSize )
         delta  ( get parameter 2 )
         ]
    ( loop [
            i  start
            sum  0.0
            ]
      ( if ( = i end )
        sum
        ( let [ x  ( * ( - i 0.5 ) delta ) ]
          ( recur ( inc i ) ( + sum ( / 1.0 ( + 1.0 ( * x x ) ) ) ) ) ) ) ) ) )

( defn execute [ numberOfThreads ]
  ( let [
         n  100000000 ; 10 times fewer due to speed issues.
         delta  ( / 1.0 n )
         startTimeNanos  ( System/nanoTime )
         sliceSize  ( / n numberOfThreads )
         pi  ( * 4.0 delta ( reduce + ( pmap partialSum  ( for [ i ( range 0 numberOfThreads ) ] [ i sliceSize delta ] ) ) ) )
         elapseTime  ( / ( - ( System/nanoTime ) startTimeNanos ) 1e9 )
         ]
    ( println ( str "==== Clojure Parallel Map pi = " pi ) )
    ( println ( str "==== Clojure Parallel Map iteration count = " n ) )
    ( println ( str "==== Clojure Parallel Map elapse = " elapseTime ) )
    ( println ( str "==== Clojure Parallel Map number of threads = " numberOfThreads ) )
    ( println ( str "==== Clojure Parallel Map number of processors = " ( .. Runtime getRuntime availableProcessors ) ) ) ) )

( execute 1 )
( println )
( execute 2 )
( println )
( execute 8 )
( println )
( execute 32 )
