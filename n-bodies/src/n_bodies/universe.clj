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
  [& particles]
  {:particles (vec particles)})

(defn random-universe
  "Creates a universe with a number of particles that have been randomly placed."
  [num-particles]
  (new-universe (for [_ (range num-particles)]
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

(defn- pow
  "Raises the number to the power of the exponent."
  [number exponent]
  (reduce * (repeat exponent number)))

(defn- average
  "Calculates the average for a collection of numbers."
  [nums]
  (/ (reduce + nums) (count nums)))

(defn- calculate-dx
  "Calculates the new dx value for this particle after colliding with the other particle."
  [this other]
  (/ (+
      (* (other :x) (pow (- (other :x) (this :x)) 2))
      (* (other :y) (- (other :x) (this :x)) (- (other :y) (this :y)))
      (* (this :dx) (pow (- (other :y) (this :y)) 2))
      (- (* (this :dy) (- (other :x) (this :x)) (- (other :y) (this :y)))))
     (+ (pow (- (other :x) (this :x)) 2) (pow (- (other :y) (this :y))))))

(defn- calculate-dy
  "Calculates the new dy value for this particle after colliding with the other particle."
  [this other]
  (/ (+
      (* (other :dx) (- (other :x) (this :x)) (- (other :y) (this :y)))
      (* (other :dy) (pow (- (other :y) (this :y)) 2))
      (- (* (this :dx) (- (other :y) (this :y)) (- (other :x) (this :x))))
      (* (this :dy) (pow (- (other :x) (this :x)) 2)))
     (+ (pow (- (other :x) (this :x)) 2) (pow (- (other :y) (this :y)) 2))))

(defn collide
  "Calculates new dx/dy values for p1 once it has collided with p2.

   This will either create or append to two new lists on the first particle. The new-dxs and
   new-dys list maintains new velocity values as collisions are calculated. These are then averaged
   at the end and the lists are removed from the particle maps."
  [p1 p2]
  {:pre (colliding? p1 p2)}
  (assoc p1
         :new-dxs (conj (p1 :new-dxs) (calculate-dx p1 p2))
         :new-dys (conj (p1 :new-dys) (calculate-dy p1 p2))))

(defn handle-collisions
  "Handles collisions for a Universe containing particles"
  [universe]
  (let [particles (universe :particles)]
    (for [particle particles
          :let [other-particles (filter #(colliding? particle %) particles)]]
      (map #(printf "Registering collision between %s and %s\n" particle %) other-particles))))

(defn step
  "Runs the universe for a single time-step, where DT time passes between snapshots."
  [universe DT]
  (if-let [after-force (calculate-forces universe)]
    (assoc after-force :particles
           (map (fn [particle] (move-particle particle DT))
                (after-force :particles)))
    {:error true}))

(defn simulate
  "Simulates running the Universe for the number of time steps associated with the Universe. The
   time between each time step is given by the DT amount associated with the Universe."
  [universe time-steps DT]
  (loop [universe universe
         i 0]
    (if (< i time-steps)
      (let [next-step (step universe DT)]
        (if (next-step :error)
          universe
          (recur next-step (inc i))))
      universe)))
