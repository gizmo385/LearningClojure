(ns automata.core
  (:require [clojure.core.typed :as t :refer [Bool ann-record Set IFn]] ))

(ann-record State [label      :- String,
                   accepting? :- Bool])
(defrecord State [label accepting?])

(defmacro defstate [name & opts]
  (let [accepting?# (and (or (opts :accepting) (opts :final)) (not (opts :non-accepting)))]
    `(def ~(symbol name) (State. ~(str name) ~accepting?#))))

(ann-record NFA
            [alphabet     :- (Set Character),
             states       :- (Set State),
             start-state  :- State
             transition-f :- (IFn [State Character -> (Set State)])])
(defrecord NFA [alphabet states start-state transition-f])

(ann-record DFA
            [alphabet     :- (Set Character),
             states       :- (Set State),
             start-state  :- State
             transition-f :- ()])
(defrecord DFA [alphabet states start-state transition-f])

; Define a DFA to accept strings following this regular expression: 0+1+
(defstate zeroes :non-accepting)
(defstate ones :accepting)
(defstate other :non-accepting)

(map #(.accepting? %) [zeroes ones other])
