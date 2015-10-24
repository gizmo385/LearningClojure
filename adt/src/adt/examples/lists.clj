(ns adt.examples.lists
  (:require [adt.core :refer [defadt adt-type defadtfn]]))

(defadt ConsList
  (Nil)
  (List car cdr))

(defadtfn car [list] list
  List (list :car)
  Nil (throw (IllegalArgumentException. "Can't get the car of an empty list!")))

(defadtfn cdr [list] list
  List (list :cdr)
  Nil (throw (IllegalArgumentException. "Can't get the cdr of an empty list!")))

(defadtfn cons [value rest] rest
  Nil (List value rest)
  List (List value rest))

(defn list [& values]
  (if (empty? values)
    (Nil)
    (List (first values) (apply list (rest values)))))

(defadtfn map [f list] list
  Nil (Nil)
  List (cons (f (car list)) (map f (cdr list))))

(defadtfn filter [pred list] list
  Nil (Nil)
  List (if (pred (car list))
         (cons (car list) (filter pred (cdr list)))
         (filter pred (cdr list))))

(defadtfn reduce [reducer initial-value list] list
  Nil initial-value
  List (reduce reducer (reducer initial-value (car list)) (cdr list)))

(defn range [start finish]
  (loop [vals (Nil)
         curr start]
    (if (not= finish curr)
      (recur (cons curr vals) (inc curr))
      (cons finish vals))))
