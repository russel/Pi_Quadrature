;  Calculation of π using quadrature. Parallel algorithm using pmap.
;
;  Copyright © 2010–2011, 2013–2015  Russel Winder

;  TODO: Find out why the very long delay between completing the computation and terminating?

(ns pi-quadrature.pi-parallelMap-javaProcessSlice)

(load "output")

(defn execute [numberOfThreads]
  (let [
         n  100000000 ; 10 times fewer than Java due to speed issues.
         delta  (/ 1.0 n)
         startTimeNanos  (System/nanoTime)
         sliceSize  (/ n numberOfThreads)
         pi  (* 4.0 delta (reduce + (pmap (fn [n] (. n compute))  (for [i (range numberOfThreads)] (ProcessSlice. i sliceSize delta)))))
         elapseTime  (/ (- (System/nanoTime) startTimeNanos) 1e9)
        ]
    (outn "Parallel Map Java ProcessSlice" pi n elapseTime numberOfThreads)))

(defn -main []
  (execute 1)
  (execute 2)
  (execute 8)
  (execute 32))
