(ns dominion.game
  "Defines how to create and manage the game state.")

;;; State templates
(def game-state-template
  {:players {}
   :piles []
   :trash []})

(def player-state-template
  "The default template for the player's in the game"
  {:victory-points 0
   :available-money 0
   :available-buys 1
   :available-actions 1
   :available-discards 0
   :available-trashes 0
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
(defn get-deck [game-state player-id]
  (get-in game-state [:players player-id :deck]))

(defn get-hand [game-state player-id]
  (get-in game-state [:players player-id :hand]))

(defn get-discard [game-state player-id]
  (get-in game-state [:players player-id :discard]))

;;; Managing the card draws for the player
(defn shuffle-discard [game-state player-id]
  (-> game-state
      (assoc-in [:players player-id :deck]
                (shuffle (get-discard game-state player-id)))
      (assoc-in [:players player-id :discard] [])))

(defn draw-from-deck [game-state player-id]
  (if (empty? (get-deck game-state player-id))
    (if (empty? (get-discard game-state player-id))
      game-state
      (draw-from-deck (shuffle-discard game-state player-id) player-id))
    (let [random-card (first (get-deck game-state player-id))]
      (as-> game-state game-state
        (update-in game-state [:players player-id :hand] conj random-card)
        (update-in game-state [:players player-id :deck] next)))))

(defn draw-n-from-deck [game-state player-id number-of-cards]
  (reduce (fn [game-state _]
            (draw-from-deck game-state player-id))
          game-state
          (range number-of-cards)))
