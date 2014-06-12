(ns wefeedus.core
  (:require [domina :as dom]
            [figwheel.client :as figw :include-macros true]
            [weasel.repl :as ws-repl]
            [hasch.core :refer [uuid]]
            [datascript :as d]
            [geschichte.stage :as s]
            [geschichte.sync :refer [client-peer]]
            [konserve.store :refer [new-mem-store]]
            [cljs.core.async :refer [put! chan <! >! alts! timeout close!] :as async]
            [cljs.reader :refer [read-string] :as read]
            [kioo.om :refer [content set-attr do-> substitute listen]]
            [kioo.core :refer [handle-wrapper]]
            [om.core :as om :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


;; TODO first prototype
;; - add clickable tooltip
;; - add input dialog
;; - filter map
;; - load marker icons

(enable-console-print!)

;; fire up repl
#_(do
    (ns weasel.startup)
    (require 'weasel.repl.websocket)
    (cemerick.piggieback/cljs-repl
        :repl-env (weasel.repl.websocket/repl-env
                   :ip "0.0.0.0" :port 17782)))


(def markers-source (ol.source.Vector. (clj->js {:features []})))


(def markers-layer (ol.layer.Vector. #js {:source markers-source}))


(def geo-map (ol.Map. (clj->js {:target "map"
                            :layers [(ol.layer.Tile.
                                      (clj->js {:source (ol.source.OSM.)}))
                                     markers-layer]
                            :view (ol.View2D.
                                   (clj->js {:center (ol.proj.transform #js [8.82 51.42]
                                                                        "EPSG:4326"
                                                                        "EPSG:3857")
                                             :zoom 13}))})))



;; todo
;; - load in templates

(defn connect-repl []
  (figw/defonce conn (ws-repl/connect "ws://localhost:17782" :verbose true)))

(.log js/console "HAIL TO THE LAMBDA!")

(def eval-fn {'(fn replace [old params] params) (fn replace [old params] params)
              '(fn [old params]
                 (:db-after (d/transact old params)))
              (fn [old params]
                (:db-after (d/transact old params)))})

#_(let [schema {:hashtags {:db/cardinality :db.cardinality/many}}
        conn   (d/create-conn schema)]
    (go (<! (s/create-repo! stage
                            "eve@polyc0l0r.net"
                            "wefeedus markers."
                            @conn
                            "master"))))


(def position (atom [8.82 51.42]))

(go-loop []
  (<! (timeout (* 60 1000)))
  (.getCurrentPosition
   (.. js/window -navigator -geolocation)
   (fn [pos] (let [coords (.-coords pos)
                  lon (.-longitude coords)
                  lat (.-latitude coords)]
              (println "You are at " lon "-" lat)
              (reset! position [lon lat])))
   #(do (js/alert "Could not fetch your geo position.")))
  (recur))


(defn add-marker [stage e]
  (let [marker-id (uuid)
        ts (js/Date.)
        user (get-in @stage [:config :user])]
    (go (<! (s/transact stage
                        ["eve@polyc0l0r.net"
                         #uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                         "master"]
                        [{:db/id marker-id
                          :pos @position
                          :user user
                          :ts ts}]
                        '(fn [old params]
                           (:db-after (d/transact old params)))))
        (<! (s/commit! stage
                       {"eve@polyc0l0r.net"
                        {#uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4" #{"master"}}})))))


;; only needed to read initial value
;; we can do this runtime wide here, since we only use this datascript version
(read/register-tag-parser! 'datascript.DB datascript/map->DB)
(read/register-tag-parser! 'datascript.Datom datascript/map->Datom)


(go
  (def store
    (<! (new-mem-store
         ;; empty db
         (atom (read-string "{#uuid \"2c7fc39d-1fb9-547a-af42-3b3e5fcb9b8f\" {:transactions [[#uuid \"0c39ae5f-0984-56e3-86c8-0434783eb2dc\" #uuid \"123ed64b-1e25-59fc-8c5b-038636ae6c3d\"]], :parents [], :ts #inst \"2014-06-08T18:55:32.654-00:00\", :author \"eve@polyc0l0r.net\"}, #uuid \"0c39ae5f-0984-56e3-86c8-0434783eb2dc\" #datascript.DB{:schema {:hashtags {:db/cardinality :db.cardinality/many}}, :ea {}, :av {}, :max-eid 0, :max-tx 536870912}, #uuid \"123ed64b-1e25-59fc-8c5b-038636ae6c3d\" (fn replace [old params] params), \"eve@polyc0l0r.net\" {#uuid \"98bac5ab-7e88-45c2-93e6-831654b9bff4\" {:branches {\"master\" #{#uuid \"2c7fc39d-1fb9-547a-af42-3b3e5fcb9b8f\"}}, :id #uuid \"98bac5ab-7e88-45c2-93e6-831654b9bff4\", :description \"wefeedus markers.\", :head \"master\", :last-update #inst \"2014-06-08T18:55:32.654-00:00\", :schema {:type \"http://github.com/ghubber/geschichte\", :version 1}, :causal-order {#uuid \"2c7fc39d-1fb9-547a-af42-3b3e5fcb9b8f\" []}, :public false, :pull-requests {}}}}"))

         (atom  {'datascript.DB datascript/map->DB
                 'datascript.Datom datascript/map->Datom}))))

  (def peer (client-peer "CLIENT-PEER" store))

  (def stage (<! (s/create-stage! "eve@polyc0l0r.net" peer eval-fn)))


  (<! (s/subscribe-repos! stage
                          {"eve@polyc0l0r.net"
                           {#uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                            #{"master"}}}))




  #_(<! (s/connect! stage "ws://localhost:8080/geschichte/ws"))

  (add-watch (-> @stage :volatile :val-atom) :marker-update
             (fn [k a o new]
               (doseq [f (.getFeatures markers-source)]
                 (.removeFeature markers-source f))
               (let [db (get-in new
                                ["eve@polyc0l0r.net"
                                 #uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                                 "master"])
                     qr (sort-by :ts
                                 (map (partial zipmap [:id :pos :user :ts])
                                      (d/q '[:find ?m ?pos ?user ?ts
                                             :where
                                             [?m :user ?user]
                                             [?m :pos ?pos]
                                             [?m :ts ?ts]]
                                           db)))]
                 (doseq [{:keys [pos user]} qr]
                   (.addFeature markers-source
                                (ol.Feature.
                                 (clj->js {:geometry (ol.geom.Point.
                                                      (ol.proj.transform (clj->js pos)
                                                                         "EPSG:4326"
                                                                         "EPSG:3857"))
                                           :user user})))))))


  (<! (timeout 500))

  (add-marker stage nil)

  #_(om/root
     (fn [stage-cursor owner]
       (om/component
        (main-view
         (let [db (get-in @stage
                          [:volatile :val
                           "eve@polyc0l0r.net"
                           #uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                           "master"])
               qr (sort-by :ts
                           (map (partial zipmap [:id :pos :user :ts])
                                (d/q '[:find ?m ?pos ?user ?ts
                                       :where
                                       [?m :user ?user]
                                       [?m :pos ?pos]
                                       [?m :ts ?ts]]
                                     db)))]
           qr)
         (partial add-marker stage))))
     stage
     {:target (. js/document (getElementById "main-container"))}))



(comment
  (get-in @stage ["eve@polyc0l0r.net" #uuid "b09d8708-352b-4a71-a845-5f838af04116"])

  (get-in @stage [:volatile :val-atom])


  [{:user "jane"
    :id 1
    :title "How to get a Designer"
    :detail-url "https://medium.com/coding-design/how-to-get-a-designer-b3afdf5a853d"
    :detail-text "Just some thoughts ..."
    :comments [{:text "awesome :D" :user "adam" :date "today"}]
    :hashtags #{"#coding" "#design"}}
   {:user "john"
    :id 2
    :title "Greenwald's 'No Place to Hide': a compelling, vital narrative about official criminality"
    :detail-text "Interesting article"
    :detail-url "http://boingboing.net/2014/05/28/greenwalds-no-place-to-hid.html"
    :comments [{:text "lies, all lies ..." :user "adam" :date "yesterday"}
               {:text "Sucker" :user "eve" :date "today"}]
    :hashtags #{"#greenwald" "#snowden" "#nsa"}}]


  (let [post-id (uuid)
        comment-id1 (uuid)
        ts (js/Date.)]
    (go (<! (s/transact stage
                        ["eve@polyc0l0r.net"
                         #uuid "fc9f725a-20eb-42f1-bb27-80d6fb2d1945"
                         "master"]
                        [{:db/id post-id
                          :title "Greenwald's 'No Place to Hide': a compelling, vital narrative about official criminality"
                          :detail-text "Interesting article"
                          :detail-url "http://boingboing.net/2014/05/28/greenwalds-no-place-to-hid.html"
                          :user "jane"
                          :ts ts}
                         {:db/id comment-id1
                          :post post-id
                          :content "awesome :D"
                          :user "adam"
                          :date "today"}]
                        '(fn [old params]
                           (:db-after (d/transact old params)))))))

  (map (partial zipmap [:id :title :detail-url :detail-text :user])
       (d/q '[:find ?p ?title ?dtext ?durl ?user
              :in $
              :where
              [?p :user ?user]
              [?p :detail-url ?durl]
              [?p :detail-text ?dtext]
              [?p :title ?title]]
            (get-in @stage [:volatile :val
                            "eve@polyc0l0r.net"
                            #uuid "b09d8708-352b-4a71-a845-5f838af04116"
                            "master"])))

  (d/q '[:find ?h ?tag
         :in $
         :where
         [?h :tag ?tag]]
       (get-in @stage [:volatile :val
                       "eve@polyc0l0r.net"
                       #uuid "b09d8708-352b-4a71-a845-5f838af04116"
                       "master"]))

  (d/q '[:find ?p (max 10 ?t)
         :in $ ?amount
         :where [?h :post ?p]
         [?h :tag ?t]]
       (get-in @stage [:volatile :val
                       "eve@polyc0l0r.net"
                       #uuid "b09d8708-352b-4a71-a845-5f838af04116"
                       "master"]))

  {:new-values {"master" {#uuid "11e35eaf-b130-5817-b679-aae174b3dcfd"
                          {:transactions [[#uuid "253cff5f-11dd-5bf9-bc9b-3e8a0c842a0b" #uuid "1f70bf3a-1d08-5cfe-a143-ee0c6c377873"]],
                           :ts #inst "2014-05-31T23:29:28.493-00:00",
                           :parents [#uuid "24a37952-42e2-5ce0-bc74-b043bb92b374"],
                           :author "eve@polyc0l0r.net"},
                          #uuid "253cff5f-11dd-5bf9-bc9b-3e8a0c842a0b"
                          ({:detail-url nil, :db/id #uuid "81cdc883-a1b3-4001-ba34-97f890aee7a9", :title "test6...", :ts #inst "2014-05-31T23:29:28.465-00:00", :detail-text "test6", :user "eve@polyc0l0r.net"}),
                          #uuid "1f70bf3a-1d08-5cfe-a143-ee0c6c377873"
                          '(fn [old params] (:db-after (d/transact old params)))}},
   :transactions {"master" []},
   :op :meta-pub,
   :meta {:branches {"master" #{#uuid "11e35eaf-b130-5817-b679-aae174b3dcfd"}},
          :id #uuid "b09d8708-352b-4a71-a845-5f838af04116",
          :description "link-collective discourse.",
          :head "master",
          :last-update #inst "2014-05-31T23:29:28.493-00:00",
          :schema {:type "http://github.com/ghubber/geschichte",
                   :version 1},
          :causal-order {#uuid "1e0604c9-a4dc-5b18-b1cc-6a75d4004364"
                         [#uuid "1dab5501-9eb1-5631-a2dc-3040410765bf"],
                         #uuid "1dab5501-9eb1-5631-a2dc-3040410765bf"
                         [#uuid "2425a9dc-7ce8-56a6-9f52-f7c431afcd91"],
                         #uuid "2425a9dc-7ce8-56a6-9f52-f7c431afcd91" [],
                         #uuid "11e976a2-133c-5418-8d85-ebdaf643b7e8"
                         [#uuid "1e0604c9-a4dc-5b18-b1cc-6a75d4004364"],
                         #uuid "010b44d4-aace-5529-a535-588543f3f13c"
                         [#uuid "11e976a2-133c-5418-8d85-ebdaf643b7e8"],
                         #uuid "24a37952-42e2-5ce0-bc74-b043bb92b374"
                         [#uuid "010b44d4-aace-5529-a535-588543f3f13c"],
                         #uuid "11e35eaf-b130-5817-b679-aae174b3dcfd"
                         [#uuid "24a37952-42e2-5ce0-bc74-b043bb92b374"]},
          :public false,
          :pull-requests {}}}
  )
