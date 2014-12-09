(ns ttt-server.core.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clojure.data.json :as json]))

; Route function declarations
(declare login)
(declare getGames)
(declare newGame)
(declare joinGame)
(declare myGame)
(declare move)

(defrecord Player
  [player_id      ; The unique identifier for this player
   game_id        ; The unique identifier for the game that this player is in
   player_type])  ; The symbol for this player, "X" or "O"

(defrecord Game
  [player_1       ; The id of the first player in the game
   player_2       ; The id of the second player in the game
   game_id        ; The id for this game
   current_turn   ; The id for the player whose turn it is
   game_state     ; The game state
   board])        ; A map of the game cells

(def players (atom {}))

(def games (atom {}))

(defroutes app-routes
  (GET "/login/:player_id" [id] login)
  (GET "/getGames" [] getGames)
  (GET "/newGame/:id" [id] newGame)
  (GET "/joinGame/:player_id/:game_id" [player_id game_id] joinGame)
  (GET "/myGame/:player_id" [] myGame)
  (GET "/move" [] move)
  (route/not-found "Not Found"))

(defn null-map [map_size]
  "Creates a map which maps the numbers from (1..map_size inclusive) to the string null"
  (loop [map {} iter 1]
    (if (<= iter map_size)
      (recur (assoc map iter "null") (inc iter))
      map)))

(defn create-game
  [player1_id]  ; The id for the first player in the game
  (let [game_id (gensym)
    game {"game_id" game_id,
    "game_state" "open",
    "players"
            {"player_id" player1_id,
            "game_id" game_id,
            "player_type" "X"},
    "you"
            {"player_id" player1_id,
            "game_id" game_id,
            "player_type" "X"},
    "player_turn"
            {"player_id" player1_id,
            "game_id" game_id,
            "player_type" "X"},
    "board" (null-map 9),
    "winner" "null"}]
    (swap! games assoc game_id game)
    game))

(defn login [args]
  )

(defn getGames [_]
  (json/write-str @games))

(defn newGame [args]
  (let [player_id (:id (:params args))
        game (create-game player_id)]
    (json/write-str game)))

(defn joinGame [args]
  (str (:params args)))

(defn myGame [client_dump]
  "myGame")

(defn move [client_dump]
  "move")

(def app
  (wrap-defaults app-routes site-defaults))
