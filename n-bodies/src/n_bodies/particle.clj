(ns n-bodies.particle
  (:use [n-bodies.utils :only [def-]]))

(def- particle-id-counter (atom 0))

(defn- new-particle-id
  "Atomically returns a new particle id."
  []
  (locking particle-id-counter
    (let [id @particle-id-counter]
      (swap! particle-id-counter inc)
      id)))

(def- default-particle-values
  {:x 0 :y 0 :dx 0 :dy 0 :forceX 0 :forceY 0 :mass 1 :radius 1 :id -1})

(defn new-particle
  "Creates a new particle. Each particle contains information about its current location and
   velocity.

   [x y] - This is current location of the particle in (x,y) space
   radius - This is the distance from the center of the particle to its edge.
   [dx dy] - This is the velocity of the particle in the x and y directions."
  ([[x y]]
   (merge default-particle-values {:x x :y y :id (new-particle-id)}))
  ([[x y] radius]
   (merge default-particle-values {:x x :y y :radius radius :id (new-particle-id)}))
  ([[x y] dx dy]
   (merge default-particle-values {:x x :y y :dx dx :dy dy :id (new-particle-id)}))
  ([[x y] radius dx dy]
   (merge default-particle-values {:x x :y y :dx dx :dy dy :radius radius :id (new-particle-id)})))

(defn move-particle
  "Returns a particle that has been moved based on its current velocity for a given number of
   seconds."
  [particle seconds]
  (let [dvX (* (/ (particle :forceX) (particle :mass)) seconds)
        dvY (* (/ (particle :forceY) (particle :mass)) seconds)
        dpX (* (+ (particle :dx) (/ dvX 2)) seconds)
        dpY (* (+ (particle :dy) (/ dvY 2)) seconds)]
    (assoc particle
           :dx (+ (particle :dx) dvX)
           :dy (+ (particle :dy) dvY)
           :x (+ (particle :x) dpX)
           :y (+ (particle :y) dpY)
           :forceX 0
           :forceY 0)))
