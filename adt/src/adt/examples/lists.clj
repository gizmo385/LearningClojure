(ns adt.examples.lists
  (:require [adt.core :refer [defadt adt-type]]))

(defadt ConsList
  (Nil)
  (List car cdr))

(defmulti car adt-type)
(defmethod car List [list] (list :car))
(defmethod car Nil [list]
  (throw (IllegalArgumentException. "Can't get the car of an empty list!")))

(defmulti cdr adt-type)
(defmethod cdr List [list] (list :cdr))
(defmethod cdr Nil [list]
  (throw (IllegalArgumentException. "Can't get the cdr of an empty list!")))

(defmulti cons (fn [value rest] (adt-type rest)))
(defmethod cons Nil [value rest] (List value rest))
(defmethod cons List [value rest] (List value rest))
(defmethod cons :default [_ other]
  (throw (IllegalArgumentException. (format "Cannot cons onto a %s" (type other)))))

(defn list [& values]
  (if (empty? values)
    (Nil)
    (List (first values) (apply list (rest values)))))

(defmulti map (fn [f list] (adt-type list)))
(defmethod map Nil [_ _] (Nil))
(defmethod map List [f list] (cons (f (car list)) (map f (cdr list))))

(defmulti filter (fn [pred list] (adt-type list)))
(defmethod filter Nil [_ _] (Nil))
(defmethod filter List [pred list]
  (if (pred (car list))
    (cons (car list) (filter pred (cdr list)))
    (filter pred (cdr list))))

(defmulti reduce (fn [reducer initial-value list] (adt-type list)))
(defmethod reduce Nil [reducer initial-value list] initial-value)
(defmethod reduce List [reducer initial-value list]
  (let [new-value (reducer initial-value (car list))]
    (reduce reducer new-value (cdr list))))

(defn range [start finish]
  (loop [vals (Nil)
         curr start]
    (if (not= finish curr)
      (recur (cons curr vals) (inc curr))
      (cons finish vals))))
