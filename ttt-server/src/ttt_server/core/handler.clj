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

(def players (atom {})) ; Maps player ids to player records
(def games (atom {}))   ; Maps game ids to game records

(defroutes app-routes
  (GET "/" []
       (fn [request]
         (let [query_args (:params request)
               player_id (-> request :session/key str)]
           ; We will dispatch based on the command in the query string
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
  (route/not-found #(format "Not Found: \"%s\"" (:uri %1))))

(defn json-response [message next-title next]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/write-str {:message message (keyword next-title) next})})

(defn fill-board [board_size fill_item]
  "Creates a map which maps the numbers from (1..board_size inclusive) to the string null"
  (loop [board {}
         iter 1]
    (if (<= iter board_size)
      (recur (assoc board iter fill_item) (inc iter))
      board)))

(defn login [player_id]
  "Logs in the user with the specified player ID"
  (if (not-nil? (get @players player_id))
    (json-response "Player Already Logged In" "playerid" player_id)
    (let [player (create-player player_id)]
      (swap! players assoc player_id player)
      (json-response "New Player Logged In" "playerid" player_id))))

(defn create-player [player_id]
  "Creates a player with a player id"
  {:player_id       player_id
   :game_id         nil
   :player_type     (rand-nth ["X" "O"])
   :last_modified   (System/currentTimeMillis)})

(defn create-game [player1_id game_id]
  "Creates a game where the specified id is the first player"
  (let [player (get @players player1_id)]
    (if (not-nil? player)
      {:game_id     game_id
       :game_state  "open"
       :players     [player]
       :you         player
       :player_turn player
       :moves       []
       :board       (fill-board 9 "null")
       :winner      nil}
      nil)))

(defn getGames [_]
  "Returns all currently open games"
  (json-response "Open Games" "data" (filter #(= "open" (:game_state %1)) (vals @games))))

(defn newGame [player_id _]
  "Creates a new game and associates the players with them"
  (let [player      (get @players player_id)
        old_game    (get @games (:game_id player))
        game_id     (gensym)
        new_player  (assoc player :game_id game_id)]
    (swap! players assoc player_id new_player)

    ; Create the game and add it to the list of games
    (let [game (create-game player_id game_id)]
      (swap! games assoc (:game_id game) game))

    ; Mark the old game as ended
    (if (not-nil? old_game)
      (swap! games assoc (:game_id old_game) (assoc old_game :game_state "ended")))
    (json-response "New game created" "gameID" game_id)))


(defn joinGame [player_id query_args]
  "joinGame")

(defn myGame [player_id _]
  "Returns the current game state for the game that the player is in"
  (let [player  (get @players player_id)
        game    (get @games (:game_id player))]
    (json-response (format "Game state for Game ID: %s" (:game_id game)) "data" game)))

(defn move [player_id query_args]
  (str "player with id \"" player_id "\" move"))

(def app
  (wrap-defaults app-routes site-defaults))
