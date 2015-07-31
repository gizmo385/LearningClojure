(ns chat-app.client
  (:use [chat-app protocol util])
  (:import [chat_app.protocol Message]
           [java.io IOException ObjectInputStream ObjectOutputStream]
           [java.net Socket UnknownHostException]))

;; Predeclare any helper functions
(declare establish-connection receive-message-as send-message-as create-server-reader
         client-name-message)

;; Message and command response handlers
(defmulti client-message-handler
  "Multi-function to handle messages that are received from the chat server"
  (fn [message] (:type message)))

;; Client-wide constants
(defonce default-port 12121)

(defn start-client
  "Starts a client that will connect to a server"
  ([client-name]
   (start-client "localhost" default-port client-name))
 
  ([hostname port client-name]
   (if-let [socket (establish-connection hostname port)]
     (let [client (merge {:socket socket :name client-name} (get-io-streams socket))
           reader-thread (Thread. (create-server-reader client))]
       (send-message-as client (client-name-message client))
       (.start reader-thread)
       client)
     nil)))

(defn- establish-connection
  "Establishes a connection to a server and returns the client data for the connection"
  [hostname port]
  (try
    (Socket. hostname port)
    (catch Exception e nil)))

(defn- client-name-message [client]
  (Message. (client :id) -1 (client :name) :login-info))

(defn- create-server-reader
  "Creates a function that will listen for messages from the server"
  [client]
  (fn []
    (with-open [socket (client :socket)
                in (client :in)]
      (let [receive (partial receive-message-as client)]
        (loop [message (receive)]
          (client-message-handler message)
          (recur (receive)))))))

(defn send-message-as
  "Sends a message as the specified client. An easy 'send' function can be created by partially
   applying this function with the client you are using."
  [client message]
  (if-let [out (client :out)]
    (try
      (.writeObject out message)
      (catch IOException e "There was an error while sending the message!"))))

(defn receive-message-as
  "Reads on the input stream for the client and returns the message retrieved. A simple 'recieve'
   function can be created by partially applying this function with the client you are recieving
   for."
  [client]
  (if-let [in (client :in)]
    (try
      (.readObject in)
      (catch IOException e "There was an error while attempting to read from the server"))))
