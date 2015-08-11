(ns adt.core
  "An implementation of Algebraic Data Types in Clojure"
  (require [clojure.string :refer [join]]
           [clojure.pprint :refer [pprint]]))

; Utility functions
(defn adt-name
  "Returns the name of the ADT that this object is a part of."
  [obj]
  (-> obj meta :adt))

(defn adt-type
  "Returns the name of the type in the ADT this was created as."
  [obj]
  (-> obj meta :adt-type))

(defn adt?
  "Returns true if this object was created by an ADT"
  [obj]
  (-> obj meta :adt boolean))

; ADT Code generation
(defn- emit-constructor [adt-name type-name & fields]
  "Generates a constructor function for a specific type in an ADT"
  (let [type-name# (symbol type-name)
        metadata {:adt (str adt-name) :adt-type type-name#}]
    (if (empty? fields)
      `(defn ~type-name# [] (with-meta {} ~metadata))
      `(defn ~type-name# [~@fields]
         (with-meta
           (struct (create-struct ~@(map keyword fields)) ~@fields)
           ~metadata)))))

(defmacro defadt [adt-name & constructors]
  "Defines an algebraic data type based on a series of constructors"
  `(do
     (defn ~(symbol (str adt-name "?")) [~'obj]
       (printf "Comparing %s and %s" adt-name (adt-name ~'obj))
       (= ~(str adt-name) (adt-name ~'obj)))
     ~@(for [[type-name & fields] constructors]
         (apply (partial emit-constructor adt-name type-name) fields))))

; Example tree ADT
(defadt Tree
  (Empty)
  (Leaf value)
  (Node value left right))

(defmulti count-nodes adt-type)
(defmethod count-nodes Empty [_] 0)
(defmethod count-nodes Leaf [leaf] 1)
(defmethod count-nodes Node [node] (+ 1 (count-nodes (node :left)) (count-nodes (node :right))))
(defmethod count-nodes :default [_] 0)

(defmulti depth adt-type)
(defmethod depth Empty [_] 0)
(defmethod depth Leaf [_] 1)
(defmethod depth Node [node] (+ 1 (max (depth (node :left)) (depth (node :right)))))
(defmethod count-nodes :default [_] 0)
