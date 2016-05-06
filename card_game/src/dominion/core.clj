(ns dominion.core
  (:require [dominion.card :as cards]
            [dominion.game :as game]))

(def default-piles
  {cards/curse    30
   cards/estate   12
   cards/duchy    12
   cards/province 12
   cards/copper   30
   cards/silver   30
   cards/gold     30
   cards/smithy   10
   cards/witch    10})

(def starting-hand
  (concat
    (repeat 3 cards/estate)
    (repeat 7 cards/copper)))

(comment
  (require '[clojure.pprint :refer [pprint]])
  (pprint starting-hand)
  (pprint
    (game/new-game-state default-piles
                         (game/new-player "Chris")
                         (game/new-player "Matt"))))
