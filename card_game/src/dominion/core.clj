(ns dominion.core
  (:require [dominion.sets.base :as base]
            [dominion.game :as game]))

(def default-piles
  {base/curse    30
   base/estate   12
   base/duchy    12
   base/province 12
   base/copper   30
   base/silver   30
   base/gold     30
   base/smithy   10
   base/witch    10})

(def starting-hand
  (concat
    (repeat 3 base/estate)
    (repeat 7 base/copper)))

(comment
  (require '[clojure.pprint :refer [pprint]])
  (pprint starting-hand)
  (pprint
    (game/new-game-state default-piles
                         (game/new-player "Chris")
                         (game/new-player "Matt"))))
