(ns automata.core
  (:require [clojure.core.typed :as t
             :refer [U Bool defalias ann ann-record Set IFn Seq Option]]))

; A state contains a label and is either an accepting state or it isn't
(ann-record State [label      :- String,
                   accepting? :- Bool])
(defrecord State [label accepting?])

(defmacro defstate
  "This allows you to create states easily, as a simple syntactic construct

   For example:
   (defstate fail)
   (defstate other-fail :non-accepting)
   (defstate pass :accepting)
   (defstate other-pass :final)

   To create an accepting state, pass in :accepting or :final after the label. Any other keywords
   will not be recognized (but may be passed for the purpose of self-documentation)
   "
  ([label]
   `(do
      (ann ~(symbol label) State)
      (def ~(symbol label) (State. ~(str label) false))))
  ([label opt] (let [accepting?# (or (= opt :accepting) (= opt :final))]
                 `(do
                    (ann ~(symbol label) State)
                    (def ~(symbol label) (State. ~(str label) ~accepting?#))))))

(defalias Automaton (U DFA))

; We represent a DFA as a 4-tuple:
; (1) A set of characters which represents the alphabet
; (2) A set of states that can be reached in the DFA
; (3) The state that the DFA will start at when testing an input string
; (4) The transition function that dictates how transitions should be made
(ann-record DFA
            [alphabet     :- (Set Character),
             states       :- (Set State),
             start-state  :- State
             transition-f :- (IFn [State Character -> State])])
(defrecord DFA [alphabet states start-state transition-f])

(ann accepts? [Automaton String -> Bool])
(defmulti accepts?
  "Given an automaton and a string, this will determine if the automaton accepts the string
   as being a part of its language."
  (t/fn [automaton :- Automaton, input :- String] :- Class (class automaton)))

(defmethod accepts? DFA [automaton input]
  (t/let [transition-f :- (IFn [State Character -> State]) (:transition-f automaton)]
    (t/loop [current :- State                      (:start-state automaton)
             sym     :- (Option Character)         (first input)
             other   :- (Option (Seq Character))   (rest input)]
      (if sym
        (recur (transition-f current sym) (first other) (rest other))
        (:accepting? current)))))


; Define a DFA to accept strings following this regular expression: 0+1+
(defstate start)
(defstate zeroes)
(defstate other)
(defstate ones :accepting)

(ann zeroes-and-ones [State Character -> State])
(defn zeroes-and-ones [state sym]
  (condp = state
    ; Start state transitions
    start (case sym
            \0 zeroes
            other)

    ; Zero state transitions
    zeroes (case sym
             \0 zeroes
             \1 ones
             other)

    ; One state transitions
    ones (case sym
           \0 other
           \1 ones
           other)

    ; Other transitions
    other other))

(def zeroes-and-ones-dfa
  (DFA. #{\0 \1} #{start zeroes ones other} start zeroes-and-ones))

(map (partial accepts? zeroes-and-ones-dfa) ["10011" "0000000000010" "01" "222"])
