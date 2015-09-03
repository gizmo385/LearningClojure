(ns adt.examples.trees
  (:require [adt.core :refer [defadt adt-type]]))

(defadt bst
  (Empty)
  (Leaf value)
  (Node left right))
