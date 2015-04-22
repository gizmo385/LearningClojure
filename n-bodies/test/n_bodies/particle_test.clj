(ns n-bodies.particle-test
  (:require [clojure.test :refer :all]
            [n-bodies.particle :refer :all]))

(def default-particle
  {:x 1 :y 1 :dx 0 :dy 0 :forceX 0 :forceY 0 :mass 1 :radius 1})

(deftest particle-creation
  (testing "Creating some particles"
    (is (= default-particle (new-particle [1 1])))
    ))
