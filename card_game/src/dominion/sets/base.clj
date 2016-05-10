(ns dominion.sets.base
  (:require [dominion.card :refer [defcard] :as card]
            [dominion.game :as game]))

;;; Victory cards
(defcard Estate "Adds a victory point." 2 :victory
  :pickup-action (card/add-victory-points 1))

(defcard Duchy "Adds 3 victory points." 5 :victory
                  :pickup-action (card/add-victory-points 3))

(defcard Province "Adds 5 victory points." 8 :victory
                  :pickup-action (card/add-victory-points 5))

(defcard Curse "Decrements your victory points by 1" 0 :victory
  :can-buy? false
  :pickup-action (card/add-victory-points -1))

;;; Treasure Cards
(defcard Copper "+$1 for buys." 0 :treasure
  :play-action (card/add-money 1))

(defcard Silver "+$2 for buys." 3 :treasure
  :play-action (card/add-money 2))

(defcard Gold "+$3 for buys." 6 :treasure
  :play-action (card/add-money 3))

;;; Kingdom cards
(defcard Witch "+2 Cards. Each other player gains a curse card." 5 :action
  :play-action (card/combine-actions (card/draw-cards 2) (card/force-card-pickup curse)))

(defcard Smithy "+3 Cards" 4 :action
  :play-action (card/draw-cards 3))

(defcard Village "+1 Card; +2 Actions" 3 :action
  :play-action (card/combine-actions (card/draw-cards 1) (card/add-actions 2)))

(defcard Festival "+2 Actions; +1 Buy; +$2" 5 :action
  :play-action (card/combine-actions (card/add-actions 2) (card/add-buys 1) (card/add-money 2)))

(defcard Laboratory "+2 Cards; +1 Action" 5 :action
  :play-action (card/combine-actions (card/draw-cards 2) (card/add-actions 1)))

(defcard Market "+1 Card; +1 Action; +1 Buy; +$1" 5 :action
  :play-action (card/combine-actions (card/draw-cards 1) (card/add-actions 1) (card/add-buys 1)
                                     (card/add-money 1)))

(defn- library-play-action
  "Implements the play action for the Library card."
  [game-state player-id]
  (letfn [(not-action-card? [card] (let [res (not= :action (:type card))] (println res) res))]
    (loop [game-state game-state]
      (if (and (>= 7 (count (game/get-hand game-state player-id)))
               (game/can-draw? game-state player-id))
        (recur (game/draw-from-deck game-state player-id not-action-card?))
        game-state))))

(defcard Library
  "Draw until you have 7 cards in hand. You may set aside any Action cards drawn this way, as you
   draw them; discard the set aside cards after you finish drawing."
  5 :action
  :play-action library-play-action)
