(ns ttt-server.core.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.session :as session]
            [clojure.data.json :as json]))

(defmacro not-nil? [& forms]
  "Returns true if none of the arguments evaluate to nil. For non-map collections, this will only
   return true if none of the elements within the collection are nil or are a collection containing
   nil."
  `(not (reduce #(or %1 %2) (map nil? (flatten (list ~@forms))))))

; Route function declarations
(declare login)
(declare getGames)
(declare newGame)
(declare joinGame)
(declare myGame)
(declare move)

(def players  (atom {}))  ; Maps player ids to player records
(def games    (atom {}))  ; Maps game ids to game records

(defroutes app-routes
  (context "" {session :session}
    (GET "/login"             []          (login session))
    (GET "/getGames"          []          (getGames session))
    (GET "/newGame"           []          (newGame session))
    (GET "/myGame"            []          (myGame session))
    (GET "/joinGame/:game-id" [game-id]   (joinGame session game-id))
    (GET "/move/:position"    [position]  (move session position)))
  (route/not-found #(format "Not found \"%s\"\n%s" (:uri %1) %1)))

(defn map-pairs [pairs]
  (if (nil? pairs) {})
  (loop [res {} pairs pairs]
    (if (empty? pairs)
      res
      (recur (assoc res (keyword (first pairs)) (second pairs)) (rest (rest pairs))))))

(defn json-response [session player_id message & others]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/write-str (assoc (map-pairs others) :message message))
     :session (assoc session :player_id player_id)})

(defn fill-board [board_size fill_item]
  "Creates a map which maps the numbers from (1..board_size inclusive) to the string null"
  (loop [board {}
         iter 1]
    (if (<= iter board_size)
      (recur (assoc board iter fill_item) (inc iter))
      board)))

(defn create-player [player_id]
  "Creates a player with a player id"
  {:player_id       player_id
   :game_id         nil
   :type            (rand-nth ["X" "O"])
   :last_modified   (System/currentTimeMillis)})

(defn create-game [player1_id game_id]
  "Creates a game where the specified id is the first player"
  (let [player (get @players player1_id)]
    (if (not-nil? player game_id)
      {:game_id     game_id
       :game_state  "open"
       :players     [player]
       :you         player
       :player_turn player
       :moves       []
       :board       (fill-board 9 "null")
       :winner      nil}
      nil)))

(defn login [session]
  (if (session :player_id)
    (json-response session (session :player_id) "Player already logged in!"
                   "player_id" (:player_id session))
    (let [player_id (gensym "player_")
          player (create-player player_id)]
      (swap! players assoc player_id player)
      (json-response session player_id "New player logged in!"
                     "player_id" player_id))))

(defn getGames [session]
  "Returns all currently open games"
  (json-response session (session :player_id) "Open Games"
                 "data" (filter #(= "open" (:game_state %1)) (vals @games))))

(defn newGame [session]
  "Creates a new game and associates the players with them"
  (let [player_id   (session :player_id)
        player      (get @players player_id)]
    (if (not-nil? player_id player)
      (let [old_game    (get @games (:game_id player))
            game_id     (gensym "game_")
            new_player  (assoc player :game_id game_id)]
        (swap! players assoc player_id new_player)

        ; Create the game and add it to the list of games
        (let [game (create-game player_id game_id)]
          (swap! games assoc (:game_id game) game))

        ; Mark the old game as ended
        (if (not-nil? old_game)
          (swap! games assoc (:game_id old_game) (assoc old_game :game_state "ended")))
        (json-response session player_id "New game created" "gameID" game_id))
      (json-response session player_id "You must login first!"))))



(defn myGame [session]
  "Returns the current game state for the game that the player is in"
  (let [player_id (session :player_id)
        player    (get @players player_id)
        game      (get @games (:game_id player))]
    (cond
      (nil? player) (json-response session player_id "You must login first!")
      (nil? game)   (json-response session player_id "You're not in a game!")
      :else (json-response session player_id (format "Game state for Game ID: %s" (:game_id game))
                                   "data" game))))

(defn joinGame [session game_id]
  (let [player_id (session :player_id)
        player    (get @players player_id)
        game      (get @games game_id)]
    ; Ensure that the player is logged in
    (if (nil? player) (json-response session player_id "You must login first!"))

    ; End there old game, if one existed
    (let [old_game_id (:game_id player)
          old_game    (get @games old_game_id)]
      (if (not-nil? old_game) (swap! games old_game_id (assoc old_game :game_state "ended"))))

    ; Add player to the new game if possible
    (if (not-nil? game)
      (let [players     (game :players)]
        (condp = (count players)
          ; If the user is joining an empty game
          0 (let [newGame   (assoc game :players (conj players player))
                  newPlayer (assoc player :game_id game_id :type (rand-nth ["X" "O"]))]
              (swap! games assoc game_id newGame)
              (swap! players assoc player_id newPlayer)
              (json-response session player_id "Joined the game. Waiting for additional player"))

          ; If one other person is playing, join and start
          1 (let [otherPlayer (first players)
                  newGame     (assoc game
                                    :players (conj players player)
                                    :game_state "playing"
                                    :player_turn (:player_id (rand-nth (players))))
                  newPlayer   (assoc player
                                    :game_id game_id
                                    :type (first (remove #(= (:type otherPlayer) %1) ["X" "Y"])))]
              (swap! games assoc game_id newGame)
              (swap! players assoc player_id newPlayer)
              (json-response session player_id "Joined the game. Waiting for additional player"))

          ; If the game is full, alert the user
          2 (json-response session player_id "The game is full!"))))))


(defn move [session position] "Endpoint")

(def app
  (wrap-defaults app-routes site-defaults))
