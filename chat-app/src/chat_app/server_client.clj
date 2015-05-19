(ns chat-app.server-client
  (:gen-class)
  (:use [chat-app protocol server]
        [clojure.string :only [join]])
  (:import [chat_app.protocol Message]))

;; Handles the list user command
(defmethod command-handler :list-users [server command]
  (let [users (join ", " (server :users))]
    (send-message (Message. (server :name) -1 users :chat))))

;; Handles forwarding general chat messages
(defmethod message-handler :chat [server message]
  (let [destination (get (server :rooms) (message :destination))]
    (map (partial send-message message) (destination :users))))

(defn -main
  [& args]
  (run-server 1212))

