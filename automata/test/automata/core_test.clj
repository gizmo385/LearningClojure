(ns automata.core-test
  (:require [clojure.test :refer :all]
            [automata.core :refer :all])
  (:import [automata.core DFA DFA-m]))

;;; Testing vars
(defstate start)
(defstate zeroes :non-accepting)
(defstate others :non-accepting)
(defstate ones :accepting)

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

(def ones-zeroes-tm {
 start {\0 zeroes :otherwise others}
 zeroes {\0 zeroes \1 ones :otherwise others}
 ones {\1 ones :otherwise others}
 others {:otherwise others}
 })

(def ones-zeroes
  (automata.core/DFA. #{\0 \1} #{start zeroes ones others} start ones-zeroes-tf))

(def ones-zeroes-m
  (automata.core/DFA-m. #{\0 \1} #{start zeroes ones others} start ones-zeroes-tm))

(deftest a-test
  (testing "State creation"
    (is (false? (:accepting? start)))
    (is (false? (:accepting? zeroes)))
    (is (false? (:accepting? others)))
    (is (true? (:accepting? ones))))

  (testing "DFA Acceptance"
    (letfn [(accepted? [s] (true? (accepts? ones-zeroes s)))
            (not-accepted? [s] (false? (accepts? ones-zeroes s)))]
      (is (not-accepted? "10011"))
      (is (not-accepted? "00000000000010"))
      (is (not-accepted? "0"))
      (is (not-accepted? ""))
      (is (not-accepted? "222"))
      (is (accepted? "01"))))

  (testing "DFA-m Acceptance"
    (letfn [(accepted? [s] (true? (accepts?-m ones-zeroes-m s)))
            (not-accepted? [s] (false? (accepts?-m ones-zeroes-m s)))]
      (is (not-accepted? "10011"))
      (is (not-accepted? "00000000000010"))
      (is (not-accepted? "0"))
      (is (not-accepted? ""))
      (is (not-accepted? "222"))
      (is (accepted? "01")))))
