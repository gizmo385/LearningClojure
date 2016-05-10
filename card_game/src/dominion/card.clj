(ns dominion.card
  "A namespace with functions used to "
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

;;; Card Creation
(defmacro defcard
  "Custom syntax for defining cards. Defines a symbol that is the lower-case of the card name and
   uses the card's description as the symbol's docstring.

   (defcard Estate \"Adds a victory point.\" 2 :victory
     :pickup-action (add-victory-points 1))

   (def estate
     \"Adds a victory point.\"
     (new-card \"Estate\" \"Adds a victory point.\" 2 :victory
       :pickup-action (add-victory-points 1)))
   "
  [name description cost type & additional-features]
  `(def ~(-> name lower-case symbol)
     ~description
     (new-card ~(str name) ~description ~cost ~type ~@additional-features)))

;;; General action functions
(defn combine-actions [& actions]
  (fn [game-state player-id]
    (reduce (fn [game-state action] (action game-state player-id)) game-state actions)))

;;; Card draw actions
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

(defn draw-cards
  "Creates an action implementation to draw a particular number of cards for a player."
  [num-cards]
  (fn [game-state player-id]
    (game/draw-n-from-deck game-state player-id num-cards)))

;;; Player stat change actions
(defn- change-player-stat
  "Updates a single player stat, such as available buys or actions."
  [stat delta]
  (fn [game-state player-id]
    (update-in game-state [:players player-id stat] + delta)))

(defn add-victory-points
  "Creates an action implementation to increase a players victory points."
  [victory-points]
  (change-player-stat :victory-points victory-points))

(defn add-actions [actions]
  "Creates an action implementation to increase a players actions"
  (change-player-stat :available-actions actions))

(defn add-money [money]
  "Creates an action implementation to increase a players money"
  (change-player-stat :available-money money))

(defn add-buys [num-buys]
  "Creates an action implementation to increase a players buys"
  (change-player-stat :available-buys num-buys))

(defn add-discards [discards]
  "Creates an action implementation to increase a players discards"
  (change-player-stat :available-discards discards))

(defn add-trashes [trashes]
  "Creates an action implementation to increase a players trashes"
  (change-player-stat :available-trashes trashes))
