;  Calculation of π using quadrature. Sequential algorithm.
;
;  Copyright © 2009–2011, 2013–2015  Russel Winder

(ns pi-quadrature.pi-sequential-loop)

(load "output")

(defn summation [count delta]
  (loop [
          i count
          s 0.0
         ]
    (if (zero? i)
      s
      (let [x (* (- i 0.5) delta)]
        (recur (dec i) (+ s (/ 1.0 (+ 1.0 (* x x)))))))))

(defn -main []
  (let [
        n  100000000 ; 10 times fewer than Java due to speed issues.
        delta  (/ 1.0 n)
        startTimeNanos  (System/nanoTime)
        pi  (* 4.0 delta (summation n delta))
        elapseTime  (/ (- (System/nanoTime) startTimeNanos) 1e9)
        ]
    (out "Sequential Loop" pi n elapseTime)))
