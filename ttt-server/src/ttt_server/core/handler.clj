(ns ttt-server.core.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clojure.data.json :as json]))

(defmacro not-nil? [& forms]
  "Returns true if none of the arguments evaluate to nil"
  `(not (reduce #(or %1 %2) (map nil? (list ~@forms)))))

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
  (GET "/" []
       (fn [client_data]
         (let [query_args (:params client_data)
               player_id (-> client_data :session/key str)]
           (case (:cmd query_args)
             "login"    (login player_id)
             "getGames" (getGames query_args)
             "newGame"  (newGame player_id query_args)
             "joinGame" (joinGame player_id query_args)
             "myGame"   (myGame player_id query_args)
             "move"     (move player_id query_args)
             (str "Unrecognized arguments: " (json/write-str query_args))))))
  (GET "/secret-data" [] #(:session/key %1))
  (GET "/all-players" [] (fn [x] (json/write-str @players)))
  (route/not-found "Not Found"))

(defn fill-board [board_size fill_item]
  "Creates a map which maps the numbers from (1..board_size inclusive) to the string null"
  (loop [board {}
         iter 1]
    (if (<= iter board_size)
      (recur (assoc board iter fill_item) (inc iter))
      board)))

(defn login [player_id]
  (if (not-nil? (get @players player_id))
    (format "Welcome back %s!" player_id)
    (let [player (Player. player_id nil nil)]
      (swap! players assoc player_id player)
      (format "Welcome new player %s" player_id))))

(defn getGames [_]
  (json/write-str @games))

(defn newGame [player_id query_args]
  "newGame")

(defn joinGame [player_id query_args]
  "joinGame")

(defn myGame [player_id query_args]
  "myGame")

(defn move [player_id query_args]
  (str "player with id \"" player_id "\" move"))

(def app
  (wrap-defaults app-routes site-defaults))
