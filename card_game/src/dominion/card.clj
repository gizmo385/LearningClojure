(ns dominion.card
  (:require [dominion.game :refer [draw-n-from-deck]]))

;; Defining cards
(def card-template
  "This is what a card encodes in our game. This template supplies some of the default values for
   the various fields that describe a card."
  {:name nil
   :description nil
   :cost 0
   :can-buy? true
   :buy-action identity
   :pickup-action identity
   :play-action identity})

(defn new-card
  "This creates a new game card. A game card has a name, a description, and an action which acts
   on the game state and the player playing the card.

   Additional features a card can have:
    * can-buy? -- Determines whether or not a card can be bought
    * buy-action -- The action performed on the game state when the card is bought
    * pickup-action -- The action performed on the game state when the card is picked up
    * play-action -- The action performed on the game state when the card is played

   All action functions are functions are two arguments. The first argument is the current game
   state. The second argument is the name of the player that is enacting the action (either
   buying, picking up, or playing a card)
   "
  [card-name description cost & {:keys [can-buy? buy-action pickup-action play-action]
                                 :as additional-features}]
  (merge
    card-template
    {:name card-name
     :description description
     :cost cost}
    additional-features))


;;; Common card actions
(defn change-victory-point-action [delta]
  (fn [game-state player-id]
    (update-in game-state [:players player-id :victory-points] (partial + delta))))

(defn- pickup-helper [game-state target-player card-to-pickup]
  (if (zero? (get (:piles game-state) card-to-pickup))
    ;; If the cards are empty, don't change the game state
    game-state
    (let [pickup-action (:pickup-action card-to-pickup)]
      (as-> game-state game-state
        (update-in game-state [:piles card-to-pickup] dec)
        (update-in game-state [:players target-player :cards] conj card-to-pickup)
        (pickup-action game-state target-player)))))

(defn force-card-pickup [card]
  (fn [game-state player-id]
    (reduce
      (fn [game-state target-player]
        (pickup-helper game-state target-player card))
      game-state
      (remove #{player-id} (keys (:players game-state))))))

;;; Some common cards
(def estate (new-card "Estate"
                      ""
                      2
                      :pickup-action (change-victory-point-action 1)))

(def duchy (new-card "Duchy"
                     ""
                     5
                     :pickup-action (change-victory-point-action 3)))

(def province (new-card "Province"
                        ""
                        8
                        :pickup-action (change-victory-point-action 6)))

(def curse (new-card "Curse"
                     "Decrements your victory points by 2."
                     0
                     :can-buy? false
                     :pickup-action (change-victory-point-action -1)))

(def witch (new-card "Witch"
                     "All other players pick up a Curse card."
                     5
                     :buy-action (force-card-pickup curse)))

(def smithy (new-card "Smithy"
                      "Draws 3 cards from your deck into your hand."
                      4
                      :play-action (fn [game-state player-id]
                                     (draw-n-from-deck game-state player-id 3))))
