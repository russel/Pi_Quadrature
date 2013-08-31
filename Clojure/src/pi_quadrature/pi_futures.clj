;  Calculation of π using quadrature. Parallel algorithm using futures.
;
;  Copyright © 2009–2011, 2013  Russel Winder

(ns pi-quadrature.pi-futures)

(load "output")
(load "partialSum")

(import '(java.util.concurrent Executors))

(defn execute [numberOfThreads]
  (let [
         n  100000000 ; 10 times fewer due to speed issues.
         delta  (/ 1.0 n)
         startTimeNanos  (System/nanoTime)
         sliceSize  (/ n numberOfThreads)
         partialSums  (map ref (replicate numberOfThreads 0))
         pool  (Executors/newFixedThreadPool numberOfThreads)
         tasks (map partialSum (map (fn [n] [n sliceSize delta]) (range numberOfThreads)))
        ]
    (doseq [future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)
    (let [
           pi  (* 4.0 delta (reduce + partialSums))
           elapseTime  (/ (- (System/nanoTime) startTimeNanos) 1e9)
          ]
      (outn "Futures" pi n elapseTime numberOfThreads))))

(defn -main []
  (execute 1)
  (execute 2)
  (execute 8)
  (execute 32))
