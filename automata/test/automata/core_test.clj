(ns automata.core-test
  (:require [clojure.test :refer :all]
            [automata.core :refer :all])
  (:import [automata.core DFA]))

;;; Define the testing states
(defstate start)
(defstate zeroes :non-accepting)
(defstate others :non-accepting)
(defstate ones :accepting)

;;; Define the transition function
(defn ones-zeroes-tf [state sym]
  (condp = state
    start (case sym
            \0 zeroes
            others)
    zeroes (case sym
             \0 zeroes
             \1 ones
             others)
    ones (case sym
           \1 ones
           others)
    others others))

;;; Define a DFA using the transition function
(def ones-zeroes
  (DFA. #{\0 \1} #{start zeroes ones others} start ones-zeroes-tf))

;;; Define a map for the transitions
(def ones-zeroes-mt {
 start {\0 zeroes :otherwise others}
 zeroes {\0 zeroes \1 ones :otherwise others}
 ones {\1 ones :otherwise others}
 others {:otherwise others}
 })

;;; Define a DFA that uses the translated transition map
(def ones-zeroes-m
  (DFA. #{\0 \1} #{start zeroes ones others} start (tmap->tfn ones-zeroes-mt)))

(deftest dfa-tests
  (testing "State creation"
    (is (false? (:accepting? start)))
    (is (false? (:accepting? zeroes)))
    (is (false? (:accepting? others)))
    (is (true? (:accepting? ones))))

  (testing "Acceptance in DFAs with transition functions"
    (letfn [(accepted? [s] (true? (accepts? ones-zeroes s)))
            (not-accepted? [s] (false? (accepts? ones-zeroes s)))]
      (is (not-accepted? "10011"))
      (is (not-accepted? "00000000000010"))
      (is (not-accepted? "0"))
      (is (not-accepted? ""))
      (is (not-accepted? "222"))
      (is (accepted? "01"))))

  (testing "Acceptance in DFAs with a translated transition map"
    (letfn [(accepted? [s] (true? (accepts? ones-zeroes-m s)))
            (not-accepted? [s] (false? (accepts? ones-zeroes-m s)))]
      (is (not-accepted? "10011"))
      (is (not-accepted? "00000000000010"))
      (is (not-accepted? "0"))
      (is (not-accepted? ""))
      (is (not-accepted? "222"))
      (is (accepted? "01")))))
