;  Calculation of π using quadrature. Parallel algorithm using pmap.
;
;  Copyright © 2009–2011, 2013–2015  Russel Winder

;  TODO: Find out why the very long delay between completing the computation and terminating?

(ns pi-quadrature.pi-parallelMap)

(load "output")

(defn partialSum [parameter]
  (let [
         index  (get parameter 0)
         sliceSize  (get parameter 1)
         start  (+ 1 (* index sliceSize))
         end  (* (+ index 1) sliceSize)
         delta  (get parameter 2)
        ]
    (loop [
            i  start
            sum  0.0
           ]
      (if (> i end)
        sum
        (let [x  (* (- i 0.5) delta)]
          (recur (inc i) (+ sum (/ 1.0 (+ 1.0 (* x x))))))))))

(defn execute [numberOfThreads]
  (let [
         n  100000000 ; 10 times fewer than Java due to speed issues.
         delta  (/ 1.0 n)
         startTimeNanos  (System/nanoTime)
         sliceSize  (/ n numberOfThreads)
         pi  (* 4.0 delta (reduce + (pmap partialSum  (for [i (range numberOfThreads)] [i sliceSize delta]))))
         elapseTime  (/ (- (System/nanoTime) startTimeNanos) 1e9)
        ]
    (outn "Parallel Map" pi n elapseTime numberOfThreads)))

(defn -main []
  (execute 1)
  (execute 2)
  (execute 8)
  (execute 32))
