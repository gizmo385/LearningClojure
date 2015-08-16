(ns adt.core-test
  (:require [clojure.test :refer :all]
            [adt.core :refer :all :exclude [cons list]]))

; Creating cons lists
(defadt ConsList
  (Nil)
  (List car cdr))

; Basic creation
(defmulti cons (fn [value rest] (adt-type rest)))
(defmethod cons Nil [value _] (List value (Nil)))
(defmethod cons List [value rest] (List value rest))

(defn list [& values]
  (if (not-empty values)
    (cons (first values) (apply list (rest values)))
    (Nil)))

; Data access
(defmulti car adt-type)
(defmethod car Nil [_] (throw (IllegalArgumentException. "Can't call car on nil")))
(defmethod car List [list] (:car list))

(defmulti cdr adt-type)
(defmethod cdr Nil [_] (throw (IllegalArgumentException. "Can't call cdr on nil")))
(defmethod cdr List [list] (:cdr list))

(deftest cons-lists
  (testing "Building a Cons-List declared as an ADT"
      (is (= (cons 1 (cons 2 (Nil)))
             (list 1 2))))
  (testing "Data accessors"
    (let [cons-list (list 1 2 3)]
      (is (= 1 (car cons-list)))
      (is (= (list 2 3) (cdr cons-list))))))

; Tree ADT with data constructor
(data Tree = Empty | Leaf value | Node value left right)

(defmulti in-order (fn [tree f] (adt-type tree)))
(defmethod in-order Empty [_ _] nil)
(defmethod in-order Leaf [leaf f] (f (leaf :value)))
(defmethod in-order Node [node f]
  (in-order (:left node) f)
  (f (node :value))
  (in-order (:right node) f))

(defmulti pre-order (fn [tree f] (adt-type tree)))
(defmethod pre-order Empty [_ _] nil)
(defmethod pre-order Leaf [leaf f] (f (leaf :value)))
(defmethod pre-order Node [node f]
  (f (node :value))
  (pre-order (:left node) f)
  (pre-order (:right node) f))

(defmulti post-order (fn [tree f] (adt-type tree)))
(defmethod post-order Empty [_ _] nil)
(defmethod post-order Leaf [leaf f] (f (leaf :value)))
(defmethod post-order Node [node f]
  (post-order (:left node) f)
  (post-order (:right node) f)
  (f (node :value)))

(deftest tree-tests
  (testing "Different tree traversals"
    (let [tree (Node 4 (Node 2 (Leaf 1) (Leaf 3)) (Node 6 (Leaf 5) (Empty)))]
      (println "In-Order")
      (in-order tree println)
      (println "Pre-Order")
      (pre-order tree println)
      (println "Post-Order")
      (post-order tree println)
      )
    )
  )
