(defproject chat-app "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [seesaw "1.4.2" :exclusions [org.clojure/clojure]]]
  :source-paths ["src" "dev"]
  :profiles {:server {:main chat-app.server-client }
             :client {:main chat-app.chat-client}})
