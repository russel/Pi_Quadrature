;  Calculation of Pi using quadrature. Parallel algorithm using pmap.
;
;  Copyright © 2010 Russel Winder

( defn execute [ numberOfThreads ]
  ( let [
         n  100000000 ; 10 times fewer due to speed issues.
         delta  ( / 1.0 n )
         startTimeNanos  ( System/nanoTime )
         sliceSize  ( / n numberOfThreads )
         pi  ( * 4.0 ( reduce + ( pmap ( fn [n] ( . n compute ) )  ( for [ i ( range 0 numberOfThreads ) ] ( new ProcessSlice [ i sliceSize delta ] ) ) ) ) )
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
