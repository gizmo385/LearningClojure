(ns chat-app.chat-client
  (:gen-class)
  (:use [chat-app client protocol util])
  (:import [chat_app.protocol Message]
           [java.io IOException]
           [java.util Scanner]))

;; Declare handler implementations

(defmethod client-message-handler :chat [message]
  (printf "%s: %s\n" (-> message :sender :name) (message :contents)))

(defmethod client-message-handler :error [message]
  (to-err
    (printf "[ERROR] %s: %s\n" (-> message :sender :name) (message :contents))))


(defn -main
  "Creates a new chat client which will connect to a running chat server"
  [& args]
  (let [client (start-client (gensym "Client"))
        send-message (partial send-message-as client)]
    (with-open [keyboard (Scanner. *in*)]
      (loop []
        (print ">")
        (let [message-contents (.nextLine keyboard)]
          (send-message (Message. (client :name) 1 message-contents :chat)))
        (recur)))))
