;  Calculation of π using quadrature. Parallel algorithm using futures.
;
;  Copyright © 2009–2011, 2013–2015  Russel Winder

(ns pi-quadrature.pi-futures)

(import '(java.util.concurrent Executors))

(load "output")

(defn partialSum [initial index sliceSize delta]
  (let [
         start  (+ 1 (* index sliceSize))
         end  (* (+ index 1) sliceSize)
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
         partialSums  (map ref (repeat numberOfThreads 0.0))
         pool  (Executors/newFixedThreadPool numberOfThreads)
         tasks (map (fn [n]
                      (fn []
                        (dosync
                         (alter (nth partialSums n) partialSum n sliceSize delta))))
                    (range numberOfThreads))
        ]
    (doseq [future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)
    (let [
           pi  (* 4.0 delta (reduce + (map deref partialSums)))
           elapseTime  (/ (- (System/nanoTime) startTimeNanos) 1e9)
          ]
      (outn "Futures" pi n elapseTime numberOfThreads))))

(defn -main []
  (execute 1)
  (execute 2)
  (execute 8)
  (execute 32))
