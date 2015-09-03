(ns adt.examples.brainfuck
  (:require [adt.core :refer [defadt adt-type]]
            [clojure.string :refer [join]]))

;; Define the different syntax objects in Brainfuck (only 7 different commands)
(defadt Brainfuck
  (Add)
  (Sub)
  (RightShift)
  (LeftShift)
  (In)
  (Out)
  (Loop commands))

(defmulti emit-c
  "Defines the conversion from Brainfuck to C"
  adt-type)
(defmethod emit-c Add [bf] "++*ptr;")
(defmethod emit-c Sub [bf] "--*ptr;")
(defmethod emit-c RightShift [bf] "++ptr;")
(defmethod emit-c LeftShift [bf] "--ptr;")
(defmethod emit-c In [bf] "*ptr=getchar();")
(defmethod emit-c Out [bf] "putchar(*ptr);")
(defmethod emit-c Loop [bf]
  (let [body (map emit-c (bf :commands))]
    (format "while(*ptr) {\n%s\n}" (join "\n" body))))

(defn- brainfuck-for-symbol
  "These are the ADT elements for non-looping symbols"
  [sym]
  (case (str sym)
    "+" (Add)
    "-" (Sub)
    ">" (RightShift)
    "<" (LeftShift)
    "." (Out)
    "," (In)))

(defn- handle-loop [string]
  (loop [loop-body [] input string]
    (let [next-token (str (first input))]
      (cond
        ; Handle errors
        (empty? next-token) (throw (IllegalArgumentException. "Unbalanced loop construct!"))

        ; Non loop characters
        (contains? #{">" "<" "+" "-" "." ","} next-token)
        (recur (conj loop-body (brainfuck-for-symbol next-token)) (rest input))

        ; Inner loops
        (= "[" next-token)
        (let [inner-loop (handle-loop (rest input))]
          (recur (conj loop-body (Loop (inner-loop :body))) (inner-loop :remaining-input)))

        ; End of loop
        (= "]" next-token) {:body loop-body :remaining-input (rest input)}))))

(defn- build-ast [string]
  (loop [input string
         ast []]
    (let [next-token (str (first input))]
      (cond
        (empty? next-token) ast

        ;; Standard symbols
        (contains? #{">" "<" "+" "-" "." ","} next-token)
        (recur (rest input) (conj ast (brainfuck-for-symbol next-token)))

        ;; Loops
        (= next-token "[")
        (let [loop-body (handle-loop (rest input))]
          (recur (loop-body :remaining-input) (conj ast (Loop (loop-body :body)))))

        ;; Otherwise
        :else (throw (IllegalArgumentException. (format "Unexpected char: %s" next-token)))))))

(defn brainfuck->c [brainfuck-source]
  (->> brainfuck-source
       (build-ast)
       (map emit-c)
       (join "\n")))
