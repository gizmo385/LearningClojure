(ns lambda-calculus.core)

(defn num->church
  "Creates a Church numeral for the supplied number n. The result with be a function with the
   following arities:

   1-arity: Takes a function f and returns a function that takes an input x and applies the
   f on x a total of n times.

   2-arity: Takes a function f and an input x. Returns the application of f onto x a total of
   n times."
  [n]
  (let [n (max n 0)]
    (fn
      ([f]
       (fn [x] ((apply comp (repeat n f)) x)))
      ([f x]
       ((apply comp (repeat n f)) x)))))

(defn church->num
  "Takes a church numeral and calculates its corresponding number."
  [church]
  ((church inc) 0))

(defn church-wrap [f]
  (fn [x] (f x) x))

(defn add
  [n m]
  (fn
    ([f]
     (fn [x] (n f (m f x))))
    ([f x]
     (n f (m f x)))))

(def succ
  (partial add (num->church 1)))

(defn mult
  "Multiplies n by m."
  [n m]
  (fn [f]
    (m (n f))))

(defn exp
  "Calculates n to the power e."
  [n e]
  (e n))
