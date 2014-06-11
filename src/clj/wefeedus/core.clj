(ns wefeedus.core
  (:gen-class :main true)
  (:require [clojure.edn :as edn]
            [net.cgrand.enlive-html :as enlive]
            [compojure.route :refer [resources]]
            [compojure.core :refer [GET POST defroutes]]
            [geschichte.repo :as repo]
            [geschichte.stage :as s]
            [geschichte.meta :refer [update]]
            [geschichte.sync :refer [server-peer client-peer]]
            [geschichte.platform :refer [create-http-kit-handler!]]
            [konserve.store :refer [new-mem-store]]
            [konserve.platform :refer [new-couch-store]]
            [compojure.handler :refer [site api]]
            [org.httpkit.server :refer [with-channel on-close on-receive run-server send!]]
            [ring.util.response :as resp]
            [cemerick.friend :as friend]
            [cemerick.friend.workflows :as workflows]
            [cemerick.friend.credentials :as creds]
            [clojure.core.async :refer [timeout sub chan <!! >!! <! >! go go-loop] :as async]
            [com.ashafa.clutch.utils :as utils]
            [com.ashafa.clutch :refer [couch]]
            [clojure.tools.logging :refer [info warn error]]))



(def behind-proxy? (or (System/getenv "SHELF_IS_BEHIND_PROXY")
                       false))

(def proto (or (System/getenv "SHELF_PROTO")
               "http"))

(def host (or (System/getenv "SHELF_HOST")
              "localhost"))

(def port (Integer.
           (or (System/getenv "SHELF_PORT")
               "8080")))

;; supply some store

(def tag-table (atom {'datascript.Datom
                      (fn [val] (info "DATASCRIPT-DATOM:" val)
                        (konserve.literals.TaggedLiteral. 'datascript.Datom val))}))

#_(reset! tag-table  {'datascript.Datom
                      (fn [val] (error "DATASCRIPT-DATOM:" val)
                        (konserve.literals.TaggedLiteral. 'datascript.Datom val))})

(def store #_(<!! (new-mem-store))
  (<!! (new-couch-store
        (couch (utils/url (utils/url (str "http://" (or (System/getenv "DB_PORT_5984_TCP_ADDR")
                                                        "localhost") ":5984"))
                          "link-collective"))
        tag-table)))


;; start synching
(def peer
  (server-peer (create-http-kit-handler! (str (if (= proto "https")
                                                "wss" "ws") "://" host ":" port "/geschichte/ws")
                                         tag-table)
               store))


(defroutes handler
  (resources "/")
  (GET "/geschichte/ws" [] (-> @peer :volatile :handler)))


(defn start-server [port]
  (do
    (info (str "Starting server @ port " port))
    (run-server (site #'handler) {:port port :join? false})))


(defn -main [& args]
  (info (first args))
  (start-server port))

(comment
  (def server (start-server 8080))
  (server))
