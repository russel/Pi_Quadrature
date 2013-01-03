;  Calculation of Pi using quadrature. Parallel algorithm using pmap.
;
;  Copyright © 2010–2011, 2013  Russel Winder

(load "output")

(defn execute [numberOfThreads]
  (let [
         n  1000000000
         delta  (/ 1.0 n)
         startTimeNanos  (System/nanoTime)
         sliceSize  (/ n numberOfThreads)
         pi  (* 4.0 delta (reduce + (pmap (fn [n] (. n compute))  (for [i (range 0 numberOfThreads)] (new ProcessSlice [i sliceSize delta])))))
         elapseTime  (/ (- (System/nanoTime) startTimeNanos) 1e9)
        ]
    (outn "Parallel Map ProcessSlice" pi n elapseTime numberOfThreads)))

(execute 1)
(execute 2)
(execute 8)
(execute 32)
