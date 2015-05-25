(ns chat-app.server-client
  (:gen-class)
  (:use [chat-app protocol server]
        [clojure.string :only [join]])
  (:import [chat_app.protocol Message]))

;; Handles the list users command
(defmethod server-command-handler :list-users [server command]
  (let [users (join ", " (server :users))]
    (send-message (Message. (server :name) -1 users :chat))))

;; Handles the list rooms command
(defmethod server-command-handler :list-rooms [server command]
  (let [rooms (join ", " (server :rooms))]
    (send-message (Message. (server :name) -1 rooms :chat))))

;; Handles forwarding general chat messages
(defmethod server-message-handler :chat [server message]
  (let [destination (get (server :rooms) (:destination message))]
    (map (partial send-message message) (destination :users))))

(defn -main
  [& args]
  (run-server 12121))
