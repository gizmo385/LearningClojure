(ns graphs.graph_test
  (:require [clojure.test :refer :all]
            [graphs.graph :refer :all]))

(def empty-graph (createGraph))

(deftest graph-creation
  (testing "Creating an empty graph"
    (let [g (createGraph)
          h (createGraph '())]
      (are [x y] (= x y)
           (.order g)   0
           (.order h)   0
           (.size g)    0
           (.size h)    0)))
  (testing "Creating a graph with a single vertex"
    (let [g (createGraph "a" "1")]
      (is (= (.order g) 1))
      (is (= (.size g) 0))))
  (testing "Variable argument graph creation"
    (let [g (createGraph "a" "1" "b" "2" "c" "3")]
      (is (= (.order g) 3))
      (is (= (.size g) 0)))))

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

(deftest adding-edges
  (let [g (createGraph "a" "1" "b" "2" "c" "3")
        h (.addEdge g "a" "b")
        k (.addEdge h "a" "c")]
    (testing "Adding some simple edges"
      (are [x y] (= x y)
           (.order g) 3
           (.order h) 3
           (.order k) 3
           (.size g)  0
           (.size h)  1
           (.size k)  2))
    (testing "Self loops are disallowed and throw an exception"
      (is (thrown? IllegalArgumentException (.addEdge g "a" "a"))))
    (testing "Duplicate edges don't get added"
      (is (= (.size (.addEdge h "a" "b")) 1)))))

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
