;  Calculation of π using quadrature. Sequential algorithm.
;
;  Copyright © 2013, 2014  Russel Winder

(ns pi-quadrature.pi-sequential-map-reduce)

(load "output")

(defn calculate [delta i]
  (let [x (* (- i 0.5) delta)]
    (/ 1.0 (+ 1.0 (* x x)))))

(defn summation [n delta]
   (reduce + (map (partial calculate delta) (range n))))

(defn -main []
  (let [
        n  10000000 ; 100 times fewer due to speed issues.
        delta  (/ 1.0 n)
        startTimeNanos  (System/nanoTime)
        pi  (* 4.0 delta (summation n delta))
        elapseTime  (/ (- (System/nanoTime) startTimeNanos) 1e9)
        ]
    (out "Sequential_Map_Reduce" pi n elapseTime)))
