(ns other.core
  (:require [clojure.java.io :refer [as-file file]]
            [loom.graph :refer [digraph add-edges]]
            [loom.attr :refer [add-attr-to-edges]]
            [loom.io :refer [view dot]]))

(defn divisibility-graph
  "Computes the divisibility graph for a particular number n. Following the edges on the graph for
   some binary number will allow you to determine whether or not that binary number is divisible
   by n"
  [n]
  (let [zero-transitions  (map (fn [i] [i (mod (* 2 i) n)])       (range n))
        one-transitions   (map (fn [i] [i (mod (inc (* 2 i)) n)]) (range n))]
    (as-> (digraph) g
      (apply add-edges g zero-transitions)
      (apply add-edges g one-transitions)
      (add-attr-to-edges g :label 0 zero-transitions)
      (add-attr-to-edges g :label 1 one-transitions))))

(comment
  (clojure.pprint/pprint (divisibility-graph 250))
  (doseq [i (range 1 3)]
    (view (divisibility-graph i)))
  )
