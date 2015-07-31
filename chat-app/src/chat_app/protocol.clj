(ns chat-app.protocol
  (:use [clojure.string :only [split]]))

(set! *warn-on-reflection* true)

;; This is the value that all implementations of this protocol will use as the ID for messages to
;; and from the server.
(defonce server-id -1)

(def message-types
  "Different types of messages that can be recieved"
  [:chat :file :audio :login-info :command :error])

(def commands
  "Types of commands that can be sent to the server"
  {"/joinroom"   :join-room
   "/createroom" :create-room
   "/leaveroom"  :leave-room
   "/listusers"  :list-users
   "/listrooms"  :list-rooms})

(def command-responses
  "Responses that can be sent from the server after a command"
  [:connection-success :login-success :login-failure :join-room-success :join-room-failure
   :leave-room-success :leave-room-failure :error])

;; A message is the base transferable type in the application
;; sender -- The name of the person sending the message
;; destination -- The id for the room that the message is being sent to
;; contents -- The object being sent in the message
;; type -- The type of message being sent
(defrecord Message [sender destination contents type])

(defn serializable?
  "Returns true if the object implements the Java Serializable interface"
  [obj]
  (instance? java.io.Serializable obj))

(defn command?
  "Returns whether or not the string message is a command (starts with a /)"
  [message-string]
  (= \/ (first message-string)))

(defn new-command
  "Returns a new message that has been formed from the command string
   sender -- The name of the person sending the message
   destination -- The room that the message is being sent to
   command-string -- This is contains a command (i.e. /joinroom testing)"
  [sender destination command-string]
  (let [command-structure (split command-string #"\s+")]
    (if-let [command-type (get commands (first command-structure))]
      (Message. sender destination {:type command-type :args (rest command-structure)} :command)
      (throw (IllegalArgumentException. "You must eneter a valid command!")))))
