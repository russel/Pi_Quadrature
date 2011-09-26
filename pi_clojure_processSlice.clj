;  Calculation of Pi using quadrature. Parallel algorithm using pmap.
;
;  Copyright © 2010–2011 Russel Winder

( defn execute [ numberOfThreads ]
  ( let [
         n  1000000000
         delta  ( / 1.0 n )
         startTimeNanos  ( System/nanoTime )
         sliceSize  ( / n numberOfThreads )
         pi  ( * 4.0 delta ( reduce + ( pmap ( fn [n] ( . n compute ) )  ( for [ i ( range 0 numberOfThreads ) ] ( new ProcessSlice [ i sliceSize delta ] ) ) ) ) )
         elapseTime  ( / ( - ( System/nanoTime ) startTimeNanos ) 1e9 )
         ]
    ( println ( str "==== Clojure Parallel Map ProcessSlice pi = " pi ) )
    ( println ( str "==== Clojure Parallel Map ProcessSlice iteration count = " n ) )
    ( println ( str "==== Clojure Parallel Map ProcessSlice elapse = " elapseTime ) )
    ( println ( str "==== Clojure Parallel Map ProcessSlice number of threads = " numberOfThreads ) )
    ( println ( str "==== Clojure Parallel Map ProcessSlice number of processors = " ( .. Runtime getRuntime availableProcessors ) ) ) ) )

( execute 1 )
( println )
( execute 2 )
( println )
( execute 8 )
( println )
( execute 32 )
