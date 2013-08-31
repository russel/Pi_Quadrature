;  Output functions for the Clojure versions of the π using Quadrature codes.
;
; Copyright © 2009, 2013  Russel Winder

(defn out [name pi n elapseTime]
  (println (str "==================== " name))
  (println (str "\tπ = " pi))
  (println (str "\titeration count = " n))
  (println (str "\telapse time = " elapseTime)))

(defn outn [name pi n elapseTime numberOfThreads]
  (out (str name ", task count: " numberOfThreads) pi n elapseTime)
  (println (str "\tnumber of processors = " (.. Runtime getRuntime availableProcessors))))
