;  Partial sum function for the Clojure versions of the π using Quadrature codes.
;
; Copyright © 2009, 2013  Russel Winder

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
      (if (= i end)
        sum
        (let [x  (* (- i 0.5) delta)]
          (recur (inc i) (+ sum (/ 1.0 (+ 1.0 (* x x))))))))))
