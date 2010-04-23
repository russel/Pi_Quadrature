;  Calculation of Pi using quadrature. Parallel algorithm using futures.
;
;  Copyright © 2009-10 Russel Winder

( import '( java.util.concurrent Executors ) )

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
         partialSums  ( map ref ( replicate numberOfThreads 0 ) )
         pool  ( Executors/newFixedThreadPool numberOfThreads )
         tasks ( map partialSum ( map ( fn [n] [ n sliceSize delta ] ) ( range numberOfThreads ) ) )
         ]
    ( doseq [ future ( .invokeAll pool tasks ) ]
      ( .get future ) )
    ( .shutdown pool )
    ( let [
           pi  ( * 4.0 ( reduce + partialSums ) )
           elapseTime  ( / ( - ( System/nanoTime ) startTimeNanos ) 1e9 )
           ]
      ( println ( str "==== Clojure Futures pi = " pi ) )
      ( println ( str "==== Clojure Futures iteration count = " n ) )
      ( println ( str "==== Clojure Futures elapse = " elapseTime ) )
      ( println ( str "==== Clojure Futures number of threads = " numberOfThreads ) )
      ( println ( str "==== Clojure Futures number of processors = " ( .. Runtime getRuntime availableProcessors ) ) ) ) ) )

( execute 1 )
( println )
( execute 2 )
( println )
( execute 8 )
( println )
( execute 32 )
