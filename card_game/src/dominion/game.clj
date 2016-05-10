(ns dominion.game
  "Defines how to create and manage the game state.")

;;; State templates
(def game-state-template
  "The basic structure of a game state for dominion"
  {:players {}
   :piles []
   :trash []})

(def player-state-template
  "The default template for the player's in the game

    * victory-points: Player with the most points wins.
    * available-money: Used to buy cards during the buy phase.
    * available-buys: The number of cards that can be bought during the buy phase.
    * available-actions: The number of actions that can be played during the action phase.
    * available-discards: The number of cards that can be discarded.
    * available-trashes: The number of cards that can be trashed.
    * free-buy-amount: The highest cost card that can be bought for free.
    * hand-limit: The highest number of cards that can be held in the hand.
    * name: The name of the player.
    * deck: The player's current deck to draw from.
    * hand: The cards that the player can play during their current turn.
    * discard: The cards that the player has discarded during play. Shuffled into deck after deck
               empties."
  {:victory-points 0
   :available-money 0
   :available-buys 1
   :available-actions 1
   :available-discards 0
   :available-trashes 0
   :free-buy-amount 0
   :hand-limit nil
   :name nil
   :deck []
   :hand []
   :discard []})

;;; Constructor functions
(defn new-player
  "Creates a new player with a specific name"
  [player-name]
  (assoc player-state-template :name player-name))

(defn new-game-state
  "Given a map of cards to numbers representing cards on the board, creates a game state. Players
   can be supplied as optional arguments"
  [piles-map & players]
  (assoc game-state-template
         :piles piles-map
         :players (apply merge (for [player players] {(java.util.UUID/randomUUID) player}))))

;;; Access functions
(defn get-deck
  "Retrieves the deck for a particular player."
  [game-state player-id]
  (get-in game-state [:players player-id :deck]))

(defn get-hand
  "Retrieves the hand for a particular player."
  [game-state player-id]
  (get-in game-state [:players player-id :hand]))

(defn get-discard
  "Retrieves the discard pile for a particular player"
  [game-state player-id]
  (get-in game-state [:players player-id :discard]))

(defn get-hand-limit
  "Retrieves the hand limit for a particular player"
  [game-state player-id]
  (get-in game-state [:players player-id :hand-limit]))

;;; Quick modification functions
(defn add-card
  "Adds a card to a particular card location (:deck, :hand :discard) for a player."
  [game-state player-id card location]
  (update-in game-state [:players player-id location] conj card))

(defn remove-card
  "Removes a card from a particular card location (:deck, :hand :discard) for a player."
  [game-state player-id location]
  (update-in game-state [:players player-id location] next))

(defn set-hand-limit
  "Retrieves the hand limit for a particular player"
  [game-state player-id hand-limit]
  (assoc-in game-state [:players player-id :hand-limit] hand-limit))

;;; Managing the card draws for the player
(defn shuffle-discard
  "Shuffles a player's discard pile and sets it as their deck. Returns the updated game state."
  [game-state player-id]
  (-> game-state
      (assoc-in [:players player-id :deck]
                (shuffle (get-discard game-state player-id)))
      (assoc-in [:players player-id :discard] [])))

(defn can-draw? [game-state player-id]
  (some not-empty [(get-deck game-state player-id)
                   (get-discard game-state player-id)]))

(defn draw-from-deck
  "Draws a single card from a players deck and adds it to their hand. Shuffles player's discard
   pile if necessary. Returns the updated game state.

   The optional keep-card? predicate can be used to discard drawn cards which do not satisfy the
   predicate."
  ([game-state player-id]
   (draw-from-deck game-state player-id (fn [card] true)))

  ([game-state player-id keep-card?]
   (if (empty? (get-deck game-state player-id))
     (if (empty? (get-discard game-state player-id))
       game-state
       (draw-from-deck (shuffle-discard game-state player-id) player-id))
     (let [random-card (first (get-deck game-state player-id))]
       (as-> game-state game-state
         (if (keep-card? random-card)
           (add-card game-state player-id random-card :hand)
           (add-card game-state player-id random-card :discard))
         (remove-card game-state player-id :deck))))))

(defn draw-n-from-deck
  "Draws a particular number of cards from a players deck and adds them to their hand. Returns the
   updated deck.

   The optional keep-card? predicate can be used to discard drawn cards which do not satisfy the
   predicate."
  ([game-state player-id number-of-cards]
   (draw-n-from-deck game-state player-id number-of-cards (fn [card] true)))

  ([game-state player-id number-of-cards keep-card?]
   (reduce (fn [game-state _] (draw-from-deck game-state player-id))
           game-state
           (range number-of-cards))))
