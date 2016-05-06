(ns cards.game
  "Defines how to create and manage the game state.")

;;; State templates
(def game-state-template
  {:players {}
   :piles []
   :trash []})

(def player-state-template
  "The default template for the player's in the game"
  {:victory-points 0
   :name nil
   :deck []
   :hand []
   :discard []})

;;; Constructor functions
(defn new-player [player-name]
  (assoc player-state-template :name player-name))

;;; Access functions
(defn get-deck [game-state player-id]
  (get-in game-state [:players player-id :deck]))

(defn get-hand [game-state player-id]
  (get-in game-state [:players player-id :hand]))

(defn get-discard [game-state player-id]
  (get-in game-state [:players player-id :discard]))

;;; Modifier functions
(defn- shuffle-discard [game-state player-id]
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
