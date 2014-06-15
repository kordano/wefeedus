(ns wefeedus.core
  (:require #_[domina :as dom]
            [figwheel.client :as figw :include-macros true]
            [weasel.repl :as ws-repl]
            [hasch.core :refer [uuid]]
            [datascript :as d]
            [datascript.btset :as btset]
            [geschichte.stage :as s]
            [geschichte.sync :refer [client-peer]]
            [konserve.store :refer [new-mem-store]]
            [cljs.core.async :refer [put! chan <! >! alts! timeout close!] :as async]
            [cljs.reader :refer [read-string] :as read]
            [kioo.om :refer [content set-attr do-> substitute listen]]
            [kioo.core :refer [handle-wrapper]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


;; TODO first prototype
;; - add clickable tooltip
;; - filter map
;; - add input dialog
;; - load marker icons from server

;; fire up repl
#_(do
    (ns weasel.startup)
    (require 'weasel.repl.websocket)
    (cemerick.piggieback/cljs-repl
        :repl-env (weasel.repl.websocket/repl-env
                   :ip "0.0.0.0" :port 17782)))

(enable-console-print!)
;; todo
;; - load in templates

(defn connect-repl []
  (figw/defonce conn (ws-repl/connect "ws://localhost:17782" :verbose true)))

(.log js/console "OIL UP!")



(defn track-position! [position geo-map]
  (go-loop []
    (.getCurrentPosition
     (.. js/window -navigator -geolocation)
     (fn [pos] (let [coords (.-coords pos)
                    lon (.-longitude coords)
                    lat (.-latitude coords)]
                (println "You are at " lon "-" lat)
                (reset! position [lon lat])
                (.setCenter (.getView geo-map) (clj->js (ol.proj.transform #js [lon lat]
                                                                           "EPSG:4326"
                                                                           "EPSG:3857")))))
     #(do (js/alert "Could not fetch your geo position.")))
    (<! (timeout (* 60 60 1000)))
    (recur)))


(defn map-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [markers-source (ol.source.Vector. #js {:features #js []})
            markers-layer (ol.layer.Vector. #js {:source markers-source})]
        {:markers-source markers-source
         :markers-layer markers-layer
         :map-id (str (gensym))
         :position (atom [0 0])}))

    om/IDidMount
    (did-mount [_]
      (let [id (om/get-state owner :map-id)
            markers (om/get-state owner :markers-layer)
            position (om/get-state owner :position)
            geo-map (ol.Map. (clj->js {:target id
                                       :layers [(ol.layer.Tile. #js {:source (ol.source.OSM.)})
                                                markers]
                                       :view (ol.View2D. #js {:center (ol.proj.transform #js [8.82 51.42]
                                                                                         "EPSG:4326"
                                                                                         "EPSG:3857")
                                                              :zoom 13})}))]
        (om/set-state! owner :map geo-map)
        (track-position! position geo-map)))

    om/IRenderState
    (render-state [this {:keys [map-id markers-source]}]
      (let [db (om/value (get-in app ["eve@polyc0l0r.net"
                                      #uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                                      "master"]))]
        (println "IRENDERSTATE" db)
        (doseq [f (.getFeatures markers-source)]
          (.removeFeature markers-source f))
        (let [qr (sort-by :ts
                          (map (partial zipmap [:id :lon :lat :user :meal-type :ts])
                               (d/q '[:find ?m ?lon ?lat ?user ?meal-type ?ts
                                      :where
                                      [?m :user ?user]
                                      [?m :lon ?lon]
                                      [?m :lat ?lat]
                                      [?m :meal-type ?meal-type]
                                      [?m :ts ?ts]]
                                    db)))]
          #_(println "QR" qr)
          (doseq [{:keys [lon lat user meal-type]} qr]
            (let [feat (ol.Feature. #js {:geometry (ol.geom.Point.
                                                    (ol.proj.transform #js [lon lat]
                                                                       "EPSG:4326"
                                                                       "EPSG:3857"))
                                         :user user})
                  icon (ol.style.Icon. #js {:anchor #js [0.5, 46]
                                            :anchorXUnits "fraction"
                                            :anchorYUnits "pixels"
                                            :opacity 0.9
                                            :src (str "img/" (name meal-type) ".png")})
                  style (ol.style.Style. #js {:image icon})]
              (.setStyle feat style)
              (.addFeature markers-source feat)))))
      (dom/div #js {:id map-id}))))


(def eval-fn {'(fn replace [old params] params) (fn replace [old params] params)
              '(fn [old params]
                 (:db-after (d/transact old params)))
              (fn [old params]
                (:db-after (d/transact old params)))})


(defn add-marker [stage user lon lat meal-type]
  (let [marker-id (uuid)
        ts (js/Date.)]
    (go (<! (s/transact stage
                        ["eve@polyc0l0r.net"
                         #uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                         "master"]
                        [{:db/id marker-id
                          :lon lon
                          :lat lat
                          :meal-type meal-type
                          :user user
                          :ts ts}]
                        '(fn [old params]
                           (:db-after (d/transact old params)))))
        (<! (s/commit! stage
                       {"eve@polyc0l0r.net"
                        {#uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4" #{"master"}}})))))

(defn read-db [{:keys [eavt aevt avet] :as m}]
  (datascript/map->DB
   (assoc m
     :eavt (apply (partial btset/btset-by d/cmp-datoms-eavt) (seq eavt))
     :aevt (apply (partial btset/btset-by d/cmp-datoms-aevt) (seq aevt))
     :avet (apply (partial btset/btset-by d/cmp-datoms-avet) (seq avet)))))

;; only needed to read initial value
;; we can do this runtime wide here, since we only use this datascript version
(read/register-tag-parser! 'datascript.DB read-db)
(read/register-tag-parser! 'datascript.Datom datascript/map->Datom)


(go
  (def store
    (<! (new-mem-store
         ;; empty db
         (atom (read-string "{#uuid \"2595c848-430f-5a42-b385-8d77fb8d2bb7\" {:transactions [[#uuid \"2928d11d-cc7e-5cc4-ad2d-cc3132c0f726\" #uuid \"123ed64b-1e25-59fc-8c5b-038636ae6c3d\"]], :parents [], :ts #inst \"2014-06-14T21:34:05.618-00:00\", :author \"eve@polyc0l0r.net\"}, #uuid \"123ed64b-1e25-59fc-8c5b-038636ae6c3d\" (fn replace [old params] params), \"eve@polyc0l0r.net\" {#uuid \"98bac5ab-7e88-45c2-93e6-831654b9bff4\" {:branches {\"master\" #{#uuid \"2595c848-430f-5a42-b385-8d77fb8d2bb7\"}}, :id #uuid \"98bac5ab-7e88-45c2-93e6-831654b9bff4\", :description \"wefeedus markers.\", :head \"master\", :last-update #inst \"2014-06-14T21:34:05.618-00:00\", :schema {:type \"http://github.com/ghubber/geschichte\", :version 1}, :causal-order {#uuid \"2595c848-430f-5a42-b385-8d77fb8d2bb7\" []}, :public false, :pull-requests {}}}, #uuid \"2928d11d-cc7e-5cc4-ad2d-cc3132c0f726\" #datascript.DB{:schema {:marker {:db/cardinality :db.cardinality/many}}, :eavt #{}, :aevt #{}, :avet #{}, :max-eid 0, :max-tx 536870912}}"))

         (atom  {'datascript.DB read-db
                 'datascript.Datom datascript/map->Datom}))))

  (def peer (client-peer "CLIENT-PEER" store))

  (def stage (<! (s/create-stage! "eve@polyc0l0r.net" peer eval-fn)))

  (<! (s/subscribe-repos! stage
                          {"eve@polyc0l0r.net"
                           {#uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                            #{"master"}}}))

  (om/root map-view (-> @stage :volatile :val-atom)
         {:target (. js/document (getElementById "map"))})

  #_(<! (s/connect! stage "ws://localhost:8080/geschichte/ws"))

  ;; some dummy state
  (add-watch (-> @stage :volatile :val-atom) :add-once
             (fn [k a o new]
               (remove-watch a k)
               (let [user (get-in @stage [:config :user])]
                 (doseq [{:keys [lon lat meal-type]}
                         [{:lon 8.485 :lat 49.455 :meal-type :soup}
                          {:lon 8.491 :lat 49.451 :meal-type :lunch}
                          {:lon 8.494 :lat 49.458 :meal-type :cake}]]
                   (add-marker stage user lon lat meal-type))))))


(comment
  ;; create new empty db in repo
  (let [schema {:marker {:db/cardinality :db.cardinality/many}}
        conn   (d/create-conn schema)]
    (go (<! (s/create-repo! stage
                            "eve@polyc0l0r.net"
                            "wefeedus markers."
                            @conn
                            "master"))))

  (d/q '[:find ?m
         :where
         [?m :meal-type ?mt]]
       (get (read-string "{\"master\" #datascript.DB{:schema {:marker {:db/cardinality :db.cardinality/many}}, :eavt #{#datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :lat, :v 49.455, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :lon, :v 8.485, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :meal-type, :v :soup, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870915, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :lat, :v 49.458, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :lon, :v 8.494, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :meal-type, :v :cake, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870913, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :lat, :v 49.451, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :lon, :v 8.491, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :meal-type, :v :lunch, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870914, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870914, :added true}}, :aevt #{#datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :lat, :v 49.455, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :lat, :v 49.458, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :lat, :v 49.451, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :lon, :v 8.485, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :lon, :v 8.494, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :lon, :v 8.491, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :meal-type, :v :soup, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :meal-type, :v :cake, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :meal-type, :v :lunch, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870914, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870914, :added true}}, :avet #{#datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :lat, :v 49.451, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :lat, :v 49.455, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :lat, :v 49.458, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :lon, :v 8.485, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :lon, :v 8.491, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :lon, :v 8.494, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :meal-type, :v :cake, :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :meal-type, :v :lunch, :tx 536870914, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :meal-type, :v :soup, :tx 536870915, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :ts, :v #inst \"2014-06-15T00:08:14.892-00:00\", :tx 536870914, :added true} #datascript.Datom{:e #uuid \"3dfa13ef-e47e-49a1-8316-9a363f060fd9\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870915, :added true} #datascript.Datom{:e #uuid \"93c62b3c-1a5f-480d-b33c-7dc5c0e29581\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870913, :added true} #datascript.Datom{:e #uuid \"d318f113-7451-44b1-8504-500329248ef5\", :a :user, :v \"eve@polyc0l0r.net\", :tx 536870914, :added true}}, :max-eid 0, :max-tx 536870915}}") "master")))
