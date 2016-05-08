(ns dominion.card
  (:require [dominion.game :as game]
            [clojure.string :refer [lower-case]]))

;; Defining cards
(def card-template
  "This is what a card encodes in our game. This template supplies some of the default values for
   the various fields that describe a card."
  {:name nil
   :description nil
   :cost 0
   :type nil
   :can-buy? true
   :trashed-on-play false
   :buy-action identity
   :pickup-action identity
   :play-action identity})

(defn new-card
  "This creates a new game card. A game card has a name, description, cost, and a card type
   (action, treasure, etc.). Additionally, they additional features which may or may not be
   different for each card.

   Additional features a card can have:
    * can-buy? -- Determines whether or not a card can be bought
    * buy-action -- The action performed on the game state when the card is bought
    * pickup-action -- The action performed on the game state when the card is picked up
    * play-action -- The action performed on the game state when the card is played
    * trashed-on-play -- Determines whether a card is trashed when played

   All action functions are functions are two arguments. The first argument is the current game
   state. The second argument is the name of the player that is enacting the action (either
   buying, picking up, or playing a card)
   "
  [card-name description cost card-type & additional-features]
  (merge
    card-template
    {:name card-name
     :description description
     :type card-type
     :cost cost}
    (apply hash-map additional-features)))


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
(defmacro defcard
  "Custom syntax for defining cards. Defines a symbol that is the lower-case of the card name and
   uses the card's description as the symbol's docstring."
  [name description cost type & additional-features]
  `(def ~(-> name lower-case symbol)
     ~description
     (new-card ~(str name) ~description ~cost ~type ~@additional-features)))

;;; Victory cards
(defcard Estate "Adds a victory point." 2 :victory
                  :pickup-action (add-victory-points 1))

(defcard Duchy "Adds 3 victory points." 5 :victory
                  :pickup-action (add-victory-points 3))

(defcard Province "Adds 5 victory points." 8 :victory
                  :pickup-action (add-victory-points 5))

(defcard Curse "Decrements your victory points by 1" 0 :victory
  :can-buy? false
  :pickup-action (add-victory-points -1))

;;; Treasure Cards
(defcard Copper "+$1 for buys." 0 :treasure
  :play-action (add-money 1))

(defcard Silver "+$2 for buys." 3 :treasure
  :play-action (add-money 2))

(defcard Gold "+$3 for buys." 6 :treasure
  :play-action (add-money 3))

;;; Kingdom cards
(defcard Witch "+2 Cards. Each other player gains a curse card." 5 :action
  :play-action (combine-actions (draw-cards 2) (force-card-pickup curse)))

(defcard Smithy "+3 Cards" 4 :action
  :play-action (draw-cards 3))

(defcard Village "+1 Card; +2 Actions" 3 :action
  :play-action (combine-actions (draw-cards 1) (add-actions 2)))

(defcard Festival "+2 Actions; +1 Buy; +$2" 5 :action
  :play-action (combine-actions (add-actions 2) (add-buys 1) (add-money 2)))

(defcard Laboratory "+2 Cards; +1 Action" 5 :action
  :play-action (combine-actions (draw-cards 2) (add-actions 1)))

(defcard Market "+1 Card; +1 Action; +1 Buy; +$1" 5 :action
  :play-action (combine-actions (draw-cards 1) (add-actions 1) (add-buys 1) (add-money 1)))
