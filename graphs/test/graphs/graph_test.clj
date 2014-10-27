(ns graphs.graph_test
  (:require [clojure.test :refer :all]
            [graphs.graph :refer :all]))

(def empty-graph (createGraph))

(deftest empty-graph-tests
  (testing "Empty graph has an empty vertex set"
    (is (= 0 (.order empty-graph))))
  (testing "Find in an empty graph is nil"
    (is (nil? (.find empty-graph "No such key")))))

(deftest adding-vertices
  (let [g (.addVertex empty-graph "Hello" "World")]
    (testing "Adding a vertex to the empty graph"
      (is (= 1 (.order g)))
      (is (= 0 (.size g))))
    (testing "Adding duplicate vertices"
      (is (thrown? IllegalArgumentException (.addVertex g "Hello World"))))
    (testing "Adding more vertices"
      (let [h (.addVertex (.addVertex (.addVertex g "a" "1") "b" "2") "c" "3")]
        (is (= 4 (.order h)))
        (is (= 0 (.size h)))))))

(deftest removing-vertices
  (let [g (.addVertex (.addVertex (.addVertex empty-graph "a" "1") "b" "2") "c" "3")
        h (.removeVertex g "a")]
    (testing "Removing vertices without edges"
      (are [expected actual] (= expected actual)
           (.order g)     3
           (.order h)     2
           (.size g)      0
           (.size h)      0
           (.find h "a")  nil ))
    (testing "Removing a vertex that doesn't exist"
      (is (thrown? IllegalArgumentException (.removeVertex empty-graph "a"))))))
