;  Calculation of π using quadrature. Parallel algorithm using pmap.
;
;  Copyright © 2009–2011, 2013  Russel Winder

;  TODO : Find out why the very long delay between completing the computation and terminating?

(load "output")

(load "partialSum")

(defn execute [numberOfThreads]
  (let [
         n  100000000 ; 10 times fewer due to speed issues.
         delta  (/ 1.0 n)
         startTimeNanos  (System/nanoTime)
         sliceSize  (/ n numberOfThreads)
         pi  (* 4.0 delta (reduce + (pmap partialSum  (for [i (range 0 numberOfThreads)] [i sliceSize delta]))))
         elapseTime  (/ (- (System/nanoTime) startTimeNanos) 1e9)
        ]
    (outn "Parallel Map" pi n elapseTime numberOfThreads)))

(execute 1)
(execute 2)
(execute 8)
(execute 32)
