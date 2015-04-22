(ns n-bodies.utils)

(defmacro def-
  "Same as def, yielding a non-public def."
  [def-name value]
  `(def ^{:private true} ~def-name ~value))
