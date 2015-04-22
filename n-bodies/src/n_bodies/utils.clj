(ns n-bodies.utils)

(defonce GRAVITY-CONSTANT 6.67e-11)

(defmacro def-
  "Same as def, yielding a non-public def."
  [def-name value]
  `(def ^{:private true} ~def-name ~value))

(defn distance
  "Returns the highest of either the distance between the two particles or the sum of the radii
   of the particles (the closest that two particles can ever truly be)."
  [p1 p2]
  (max
    (+ (p1 :radius) (p2 :radius))
    (Math/sqrt (+ (Math/pow (- (p2 :y) (p1 :y)) 2) (Math/pow (- (p2 :x) (p1 :x)) 2)))))

(defn calculate-force
  "Calculates the force between two particles in space."
  [p1 p2]
  (let [d (distance p1 p2)
        magnitude (/ (* GRAVITY-CONSTANT (p1 :mass) (p2 :mass)) (* d d))
        dirX (- (p2 :x) (p1 :x))
        dirY (- (p2 :y) (p1 :y))]
    (assoc p1
           :forceX (+ (p1 :forceX) (/ (* magnitude dirX) d))
           :forceY (+ (p1 :forceY) (/ (* magnitude dirY) d)))))
