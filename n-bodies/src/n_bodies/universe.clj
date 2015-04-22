(ns n-bodies.universe
  (:use [clojure.pprint]
        [n-bodies.particle]))

(defn- rand-in-range
  "Returns a random number that is at least the low value, but at most the high value."
  [low high]
  (+ (rand (+ (- high low) 1)) low))

(defn new-universe
  "Creates a new universe"
  [time-steps DT & particles]
  (let [particle*    (apply vec particles)]
    {:time-steps time-steps
     :DT DT
     :particles (zipmap (range (count particle*)) particle*)}))

(defn random-universe
  "Creates a universe with a number of particles that have been randomly placed."
  [time-steps DT num-particles]
  (new-universe time-steps DT
                (for [_ (range num-particles)]
                  (new-particle [(rand-in-range -100 100) (rand-in-range -100 100)]))))


(defn calculate-forces
  "Calculates forces between particles in the Universe and returns a new Universe"
  [universe])

(comment
  (pprint ((random-universe 1 1 2) :particles)))
