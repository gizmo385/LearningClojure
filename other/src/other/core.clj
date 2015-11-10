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

(defn divisibility-map
  [n]
  (zipmap
    (range n)
    (for [i (range n)]
      {0 (mod (* 2 i) n)
       1 (mod (inc (* 2 i)) n)})))

(defn modulus
  "Computes m mod n by traversing n, which should be n's divisibility map."
  [m n]
  (let [bits (map (comp (fn [i] (Integer/parseInt i)) str) (Integer/toBinaryString m))]
    (loop [current-state 0 [first-bit & remaining-bits] bits]
      (if first-bit
        (recur (get (get n current-state) first-bit) remaining-bits)
        current-state))))

(defn divisible?
  "Determines if m is divisible by n by computing n's divisibility map and then following it.
   For this to work, m is assumed to be a bit string."
  [m n]
  (zero? (modulus m (divisibility-map n))))

(defn mark-divisible
  "Marks numbers divisible by n."
  [numbers n]
  (let [graph (divisibility-map n)]
    (zipmap
      numbers
      (map (comp zero? (fn [i] (modulus i graph))) numbers))))

(comment
  ;; Finding numbers in [0 .. 100] that are divisble by 10
  (filter (fn [[n divisibility]] divisibility) (mark-divisible (range 0 101) 10))
  (clojure.pprint/pprint (into (sorted-map) (mark-divisible (range 0 101) 10)))

  ;; Testing divisibility and modulus operations
  (divisible? 325 7)  ; --> False
  (divisible? 329 7)  ; --> True
  (modulus 329 7)     ; --> 0
  (modulus 325 7)     ; --> 3

  ;; Computing divisibility maps and graphs
  (divisibility-map 5)
  (clojure.pprint/pprint (divisibility-graph 250))

  ;; Generates a graphviz visualization for the divisibility graph of a number
  (view (divisibility-graph 50))
  )
