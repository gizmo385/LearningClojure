(ns n-bodies.particle
  (:use [n-bodies.utils :only [def-]]))

(def GRAVITY-CONSTANT 6.67e-11)

(def- default-particle-values
  {:x 0 :y 0 :dx 0 :dy 0 :forceX 0 :forceY 0 :mass 1 :radius 1})

(defn new-particle
  "Creates a new particle. Each particle contains information about its current location and
   velocity.

   [x y] - This is current location of the particle in (x,y) space
   radius - This is the distance from the center of the particle to its edge.
   [dx dy] - This is the velocity of the particle in the x and y directions."
  ([[x y]]
   (merge default-particle-values {:x x :y y}))
  ([[x y] radius]
   (merge default-particle-values {:x x :y y :radius radius}))
  ([[x y] dx dy]
   (merge default-particle-values {:x x :y y :dx dx :dy dy}))
  ([[x y] radius dx dy]
   (merge default-particle-values {:x x :y y :dx dx :dy dy :radius radius})))

(defn move-particle
  "Returns a particle that has been moved based on its current velocity for a given number of
   seconds."
  [particle seconds]
  (let [dvX (* (/ (particle :forceX) (particle :mass)) seconds)
        dvY (* (/ (particle :forceY) (particle :mass)) seconds)
        dpX (* (+ (particle :velocityX) (/ dvX 2)) seconds)
        dpY (* (+ (particle :velocityY) (/ dvY 2)) seconds)]
    (assoc particle
           :velocityX (+ (particle :velocityX) dvX)
           :velocityY (+ (particle :velocityY) dvY)
           :x (+ (particle :x) dpX)
           :y (+ (particle :y) dpY)
           :forceX 0
           :forceY 0)))

(defn distance
  "Returns the highest of either the distance between the two particles or the sum of the radii
   of the particles (the closest that two particles can ever truly be."
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
    [(assoc p1 :x (/ (* (+ (p1 :forceX) magnitude) dirX) d)
            :y (/ (* (+ (p1 :forceY) magnitude) dirY) d))
     (assoc p2
            :x (/ (* (- (p2 :forceX) magnitude) dirX) d)
            :y (/ (* (- (p2 :forceY) magnitude) dirY) d))]))
