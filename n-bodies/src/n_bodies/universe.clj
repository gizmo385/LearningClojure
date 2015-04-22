(ns n-bodies.universe
  (:use [clojure.pprint]
        [n-bodies particle utils]))

(defn- rand-in-range
  "Returns a random number that is at least the low value, but at most the high value."
  [low high]
  (+ (rand (+ (- high low) 1)) low))

(defn new-universe
  "Creates a new Universe. Each Universe contains map of integers to particles where the integer
   represents the particle's ID"
  [time-steps DT & particles]
  {:time-steps time-steps
   :DT DT
   :particles (apply vec particles)})

(defn random-universe
  "Creates a universe with a number of particles that have been randomly placed."
  [time-steps DT num-particles]
  (new-universe time-steps DT
                (for [_ (range num-particles)]
                  (new-particle [(rand-in-range -5 5) (rand-in-range -5 5)]))))


(defn calculate-force-for-particle
  "Calculates forces on this particle relative to other particles and returns it"
  [universe particle]
  (loop [particle particle
         others (universe :particles)]
    (if (empty? others)
      particle
      (if (= ((first others) :id) (particle :id))
        (recur particle (rest others))
        (recur (calculate-force particle (first others)) (rest others))))))

(defn calculate-forces
  "Calculates the forces for all of the particles in the Universe and returns the modified
   universe."
  [universe]
  (let [new-particles (map (partial calculate-force-for-particle universe) (universe :particles))]
    (assoc universe :particles (into [] new-particles))))

(defn step
  [universe]
  (if-let [after-force (calculate-forces universe)]
    (assoc after-force :particles
           (map (fn [particle] (move-particle particle (universe :DT)))
                (after-force :particles)))
    {:error true}))

(defn simulate
  "Simulates running the Universe for the number of time steps associated with the Universe. The
   time between each time step is given by the DT amount associated with the Universe."
  [universe]
  (loop [universe universe
         i 0]
    (if (< i (universe :time-steps))
      (recur (step universe) (inc i))
      universe)))
