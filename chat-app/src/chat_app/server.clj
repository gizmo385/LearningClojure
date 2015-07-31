(ns chat-app.server
  (:use [chat-app protocol util])
  (:import [chat_app.protocol Message]
           [java.net ServerSocket]
           [java.io IOException ObjectInputStream ObjectOutputStream]))

;; Pre-declare helper functions
(declare send-message receieve-message error-message accept-client! create-client-reader
         add-client! add-client-to-room remove-client! create-room! remove-user-from-room)

;; A representation for a client in this server
;; socket -- The socket that the client's connection is being made on
;; out -- The stream that messages can be written to
;; in -- The stream that messages can be read from
;; id -- A unique identifier for this client
;; client-name -- The name of the client
(defn create-client [socket out in id client-name]
  (zipmap [:socket :out :in :id :client-name] [socket out in id client-name]))

;; A representation for an individual room in the server
;; room-name -- The name for this room
;; id -- The room's globally unique identifer
;; users -- The set of users recieving messages from this room
(defn create-room [room-name id users]
  (zipmap [:room-name :id :users] [room-name id users]))

;; The ID for the global room which all clients automatically join
(defonce global-room-id 1)

(def server-state
  "The server state is a map which contains the current connected users, rooms open, and the
   server name"
  (atom {:users {}
         :rooms {global-room-id (create-room "Global Room" global-room-id [])}
         :name "Server"}))

;;; Message and Command Handlers

(defmulti server-command-handler
  "Multi-function to handle different types of server commands."
  (fn [server command] (:type command)))

(defmethod server-command-handler :default [_ command]
  (let [error (format "Command type \"%s\" cannot be handled!" (command :type))]
    (send-message (error-message error) (command :sender))))

(defmulti server-message-handler
  "Multi-function to handle messages coming from users."
  (fn [server message] (:type message)))

(defmethod server-message-handler :default [_ message]
  (let [error (format "Message type \"%s\" cannot be handled!" (message :type))]
    (send-message (error-message error) (message :sender))))

;; This message handler will automatically hand commands off to its proper command handler
(defmethod server-message-handler :command [server message]
  (server-command-handler server (message :contents)))

;;; Running the server and managing clients

(defn run-server
  "Listens on a particular port for incoming clients. Messages from those clients will be handled
   by calling the handlers that were supplied to this function.

   port -- The port number that the server socket will listen on"
  [port]
  ;; Open the server connection
  (with-open [server (ServerSocket. port)]
    (printf "Running server on port %d\n" port)
    (loop []
      (println "Waiting for client to connect...")
      (let [socket (.accept server)]
        ;; Wait to accept a client on the server
        (printf "Client has connected from %s:%d\n" (.getInetAddress socket) (.getPort socket))
        (if-let [client (accept-client! socket)]
          ;; Start a thread for the new client
          (let [handler (Thread. (create-client-reader client))]
            (.start handler))))
      (recur))))

(defn accept-client!
  "Accepts a new client on the socket new-client. Returns the new clients map which maps client's
   id numbers to their client map. A client map contains the following information:

   client-name: The name of the client
   socket: The socket that the connection is made on
   out: The stream to write messages to
   in: The stream to read messages from
   id: The unique identifier for this client"
  [client-socket]
  (if-let [streams (get-io-streams client-socket)]
    (let [client-name (:contents (receieve-message streams))
          id (uuid)]
      (if (string? client-name)
        (let [new-client (create-client client-socket (streams :out) (streams :in) id client-name)]
          (add-client! new-client)
          new-client)))))

(defn create-room!
  "Creates a new room and assigns it a unique identifier. The room will be added to the global map
   and the new room will be returned from the function."
  [room-name]
  (let [id (uuid)
        room (create-room room-name id #{})
        current-rooms (get @server-state :rooms)]
    (swap! server-state assoc :rooms (assoc current-rooms id room))
    room))

(defn create-client-reader
  "Returns a function that will be used to monitor a clients connection and handle messages."
  [client]
  (fn []
    (loop [message (receieve-message client)]
      (let [dest (get (@server-state :rooms) (:destination message))]
        (cond
          ;; If an error occurs while reading, remove the client from the server
          (contains? message :error)
          (remove-client! client)

          ;; Send a message to the client if they try to send to a non-existent room
          (nil? dest)
          (do
            (send-message
              (error-message (format "\"%d\" is not a valid room" (:destination message)))
              client)
            (recur (receieve-message client)))

          ;; Pass the message on to the proper message handler
          :default
          (do
            (server-message-handler @server-state message)
            (recur (receieve-message client))))))))


(defn receieve-message
  "Reads a message from a client's input stream"
  [{:keys [in] :as client}]
  (let [in (client :in)]
    (try
      (.readObject in)
      (catch IOException e {:contents nil :error "Error reading message!"}))))

(defn send-message
  "Writes a message to a client's output stream"
  [message to]
  (cond
    ;; If the user id was passed
    (integer? to)
    (let [users (@server-state :users)]
      (send-message message (get users to)))

    ;; If the client record was passed
    (and (map? to) (contains? to :out))
    (let [out (to :out)]
    (try
      (.writeObject out message)
      (catch IOException e nil)))

    :default
    (throw (IllegalArgumentException. "2nd argument must be either an int or a map with :out"))))

(defn add-client! [client]
  "Adds a client to the server"
  (let [current-users (get @server-state :users)
        current-rooms (get @server-state :rooms)
        global-room (get current-rooms global-room-id)]
    (swap! server-state assoc
           :users (conj current-users client)
           :rooms (assoc current-rooms global-room-id (add-client-to-room client global-room)))))

(defn add-client-to-room
  "Adds a client to the room"
  [client room]
  (let [current-users (room :users)]
    (assoc room :users (conj current-users client))))

(defn remove-client-from-room
  "Removes a client from a room."
  [client room]
  (assoc room :users (filter (partial not= client) (room :users))))

(defn remove-client!
  "Removes a client from the server and from any rooms they are connected to"
  [client]
  (let [current-users (get @server-state :users)
        current-rooms (get @server-state :rooms)]
    (swap! server-state assoc
           :users (dissoc current-users (client :id))
           :rooms (map (partial remove-client-from-room client) current-rooms))))

;;; Utility functions

(defn- error-message
  "Creates an error message that is sent from the server"
  [message-text]
  (Message. (@server-state :server-name) -1 message-text :error))
