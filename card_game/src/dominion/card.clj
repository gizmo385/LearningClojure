(ns dominion.card
  (:require [dominion.game :as game]))

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
(defn combine-actions [& actions]
  (fn [game-state player-id]
    (reduce (fn [game-state action] (action game-state player-id)) game-state actions)))

(defn- pickup-helper [game-state target-player card-to-pickup]
  (if (zero? (get (:piles game-state) card-to-pickup))
    ;; If the cards are empty, don't change the game state
    game-state
    (let [pickup-action (:pickup-action card-to-pickup)]
      (as-> game-state game-state
        (update-in game-state [:piles card-to-pickup] dec)
        (update-in game-state [:players target-player :cards] conj card-to-pickup)
        (pickup-action game-state target-player)))))

(defn force-card-pickup
  "Forces everyone besides the current player to pick up a card."
  [card]
  (fn [game-state player-id]
    (reduce
      (fn [game-state target-player]
        (pickup-helper game-state target-player card))
      game-state
      (remove #{player-id} (keys (:players game-state))))))

(defn- change-player-stat
  "Updates a single player stat, such as available buys or actions."
  [stat delta]
  (fn [game-state player-id]
    (update-in game-state [:players player-id stat] + delta)))

(defn draw-cards [num-cards]
  (fn [game-state player-id]
    (game/draw-n-from-deck game-state player-id num-cards)))

(defn add-victory-points [victory-points]
  (change-player-stat :victory-points victory-points))

(defn add-actions [actions]
  (change-player-stat :available-actions actions))

(defn add-money [money]
  (change-player-stat :available-money money))

(defn add-buys [num-buys]
  (change-player-stat :available-buys num-buys))

(defn add-discards [discards]
  (change-player-stat :available-discards discards))

(defn add-trashes [trashes]
  (change-player-stat :available-trashes trashes))

;;; Card definitions
;;; Victory cards
(def estate (new-card "Estate"
                      ""
                      2
                      :pickup-action (add-victory-points 1)))

(def duchy (new-card "Duchy"
                     ""
                     5
                     :pickup-action (add-victory-points 3)))

(def province (new-card "Province"
                        ""
                        8
                        :pickup-action (add-victory-points 6)))

(def curse (new-card "Curse"
                     "Decrements your victory points by 2."
                     0
                     :can-buy? false
                     :pickup-action (add-victory-points -1)))

;;; Treasure Cards
(def copper (new-card "Copper"
                      ""
                      0
                      :play-action (add-money 1)))

(def silver (new-card "Silver"
                      ""
                      0
                      :play-action (add-money 3)))

(def gold (new-card "Gold"
                      ""
                      0
                      :play-action (add-money 5)))
;;; Kingdom cards
(def witch (new-card "Witch"
                     "All other players pick up a Curse card."
                     5
                     :play-action (combine-actions (draw-cards 2) (force-card-pickup curse))))

(def smithy (new-card "Smithy"
                      "Draws 3 cards from your deck into your hand."
                      4
                      :play-action (draw-cards 3)))

(def village (new-card "Village"
                       "+1 Card; +2 Actions"
                       3
                       :play-action (combine-actions (draw-cards 1) (add-actions 2))))

(def festival (new-card "Festival"
                        "+2 Actions; +1 Buy; +$2"
                        5
                        :play-action (combine-actions (add-actions 2) (add-buys 1) (add-money 2))))

(def laboratory (new-card "Laboratory"
                          "+2 Cards; +1 Action"
                          5
                          :play-action (combine-actions (draw-cards 2) (add-actions 1))))

(def market (new-card "Market"
                      "+1 Card; +1 Action; +1 Buy; +$1"
                      5
                      :play-action (combine-actions (draw-cards 1) (add-actions 1) (add-buys 1)
                                     (add-money 1))))
