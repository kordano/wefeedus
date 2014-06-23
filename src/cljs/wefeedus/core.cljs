(ns wefeedus.core
  (:require #_[domina :as dom]
            [jayq.core :refer [$ text]]
            [wefeedus.view :refer [app-view map-popup]]
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


(defn add-markers! [markers-vector markers]
  (doseq [f (.getFeatures markers-vector)]
    (.removeFeature markers-vector f))
  (doseq [{:keys [lon lat user meal-type description]} markers]
    (let [feat (ol.Feature. #js {:geometry (ol.geom.Point.
                                            (ol.proj.transform #js [lon lat]
                                                               "EPSG:4326"
                                                               "EPSG:3857"))
                                 :user user
                                 :description description})
          icon (ol.style.Icon. #js {:anchor #js [0.5, 0.5]
                                    :anchorXUnits "fraction"
                                    :anchorYUnits "pixels"
                                    :opacity 0.9
                                    :src (str "img/" (name meal-type) ".png")})
          style (ol.style.Style. #js {:image icon})]
      (.setStyle feat style)
      (.addFeature markers-vector feat))))


(defn map-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [markers-source (ol.source.Vector. #js {:features #js []})
            markers-layer (ol.layer.Vector. #js {:source markers-source})]
        {:markers-source markers-source
         :markers-layer markers-layer
         :map-id "map"
         :position (atom [0 0])
         :start-ts (js/Date.)
         :end-ts (js/Date. (+ (.getTime (js/Date.)) (* 6 60 60 1000)))}))

    om/IDidMount
    (did-mount [_]
      (let [id (om/get-state owner :map-id)
            markers (om/get-state owner :markers-layer)
            position (om/get-state owner :position)
            start-ts-ch (om/get-state owner :start-ts-ch)
            end-ts-ch (om/get-state owner :end-ts-ch)
            geo-map (ol.Map. #js {:target id
                                  :layers #js [(ol.layer.Tile. #js {:source (ol.source.OSM.)})
                                               markers]
                                  :view (ol.View2D. #js {:center (ol.proj.transform #js [8.82 51.42]
                                                                                    "EPSG:4326"
                                                                                    "EPSG:3857")
                                                         :zoom 13})})
            popup (.getElementById js/document "map-popup")
            popup-overlay (ol.Overlay. #js {:element popup
                                            :positioning "bottom-center"
                                            :stop-event false})
            start-ts-ch (om/get-state owner :start-ts-ch)
            end-ts-ch (om/get-state owner :end-ts-ch)
            date-ch (om/get-state owner :date-ch)]
        (go-loop [s (<! start-ts-ch)]
          (om/set-state! owner :start-ts s)
          (recur (<! start-ts-ch)))
        (go-loop [e (<! end-ts-ch)]
          (om/set-state! owner :end-ts e)
          (recur (<! end-ts-ch)))
        (.addOverlay geo-map popup-overlay)
        (om/set-state! owner :map geo-map)
        (om/set-state! owner :popup popup-overlay)
        (track-position! position geo-map)
        (.on geo-map "click"
             (fn [e] (if-let [feat (.forEachFeatureAtPixel geo-map (.-pixel e) (fn [feat lay] feat))]
                      (do
                        (.popover ($ popup) "destroy")
                        (.setPosition popup-overlay (.. feat getGeometry getCoordinates))
                        (.popover ($ popup) #js {:placement "top"
                                                 :html true
                                                 :content (map-popup (.get feat "user")
                                                                     (.get feat "description"))})
                        (.popover ($ popup) "show"))
                      (.popover ($ popup) "destroy"))))))

    om/IRenderState
    (render-state [this {:keys [map-id markers-source start-ts end-ts]}]
      (let [db (om/value app)]
        #_(println "IRENDERSTATE" start-ts end-ts)
        (let [qr (sort-by :ts
                          (map (partial zipmap [:id :lon :lat :user :description :meal-type :ts :start :end])
                               (d/q '[:find ?m ?lon ?lat ?user ?descr ?meal-type ?ts ?start ?end
                                      :in $ $start-ts $end-ts %
                                      :where
                                      [?m :user ?user]
                                      [?m :description ?descr]
                                      [?m :lon ?lon]
                                      [?m :lat ?lat]
                                      [?m :meal-type ?meal-type]
                                      [?m :start-ts ?start]
                                      [?m :end-ts ?end]
                                      [?m :ts ?ts]
                                      [in-range ?start ?end $start-ts $end-ts]]
                                    db start-ts end-ts
                                    '[[(in-range ?start ?end ?start-ts ?end-ts)
                                       [(> ?start ?start-ts)]
                                       [(< ?start ?end-ts)]]
                                      [(in-range ?start ?end ?start-ts ?end-ts)
                                       [(> ?end ?start-ts)]
                                       [(< ?end ?end-ts)]]
                                      [(in-range ?start ?end ?start-ts ?end-ts)
                                       [(> ?start ?start-ts)]
                                       [(< ?end ?end-ts)]]
                                      [(in-range ?start ?end ?start-ts ?end-ts)
                                       [(< ?start ?start-ts)]
                                       [(> ?end ?end-ts)]]])))]
          #_(println "QR" qr)
          (add-markers! markers-source qr)))
      (dom/div #js {:id map-id}
               (dom/div #js {:id "map-popup"})))))


(defn app [[user id br] stage owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [d (js/Date.)]
        {:start-ts-ch (chan)
         :end-ts-ch (chan)
         :date d
         :start-time #js {:hours (.getHours d)
                          :minutes (* (inc (int (/ (.getMinutes d) 15))) 15)}
         :end-time #js {:hours (inc (.getHours d))
                        :minutes (* (inc (int (/ (.getMinutes d) 15))) 15)}}))
    om/IDidMount
    (did-mount [_]
      (let [start-ts-ch (om/get-state owner :start-ts-ch)
            end-ts-ch (om/get-state owner :end-ts-ch)
            update-fn (fn []
                        (let [d (om/get-state owner :date)
                              s (om/get-state owner :start-time)
                              e (om/get-state owner :end-time)
                              o (.getTimezoneOffset (js/Date.))
                              st (+ (.getTime d)
                                    (* 1000 60 60 (.-hours s))
                                    (* 1000 60 (- (.-minutes s) o)))
                              et (+ (.getTime d)
                                    (* 1000 60 60 (.-hours e))
                                    (* 1000 60 (- (.-minutes e) o)))]
                          (put! start-ts-ch (js/Date. st))
                          (put! end-ts-ch (js/Date. et))))]
        (.datepicker ($ :#datepicker)
                     #js {:autoclose true
                          :todayHighlight true})
        (.on ($ :#datepicker)
             "changeDate"
             (fn [e]
               (om/set-state! owner :date (.-date e))
               (update-fn)))

        (.timepicker ($ :#start-time-picker)
                     #js {:minuteStep 15
                          :showMeridian false})
        (.timepicker ($ :#start-time-picker) "setTime" (let [e (om/get-state owner :start-time)
                                                             h (.-hours e)
                                                             m (.-minutes e)]
                                                         (str (if (< h 10) (str "0" h) h)
                                                              ":"
                                                              (if (< m 10) (str "0" m) m))))

        (.on ($ :#start-time-picker)
             "changeTime.timepicker"
             (fn [e]
               (om/set-state! owner :start-time (.-time e))
               (update-fn)))

        (.timepicker ($ :#end-time-picker)
                     #js {:minuteStep 15
                          :showMeridian false})
        (.timepicker ($ :#end-time-picker) "setTime" (let [e (om/get-state owner :end-time)
                                                           h (.-hours e)
                                                           m (.-minutes e)]
                                                       (str (if (< h 10) (str "0" h) h)
                                                            ":"
                                                            (if (< m 10) (str "0" m) m))))
        (.on ($ :#end-time-picker)
             "changeTime.timepicker"
             (fn [e]
               (om/set-state! owner :end-time (.-time e))
               (update-fn)))
        (update-fn)))

    om/IRenderState
    (render-state [this {:keys [start-ts-ch end-ts-ch]}]
      (app-view (om/build map-view
                          (get-in stage [user id br])
                          {:init-state {:start-ts-ch start-ts-ch
                                        :end-ts-ch end-ts-ch}})))))


(def eval-fn {'(fn replace [old params] params) (fn replace [old params] params)
              '(fn [old params]
                 (:db-after (d/transact old params)))
              (fn [old params]
                (:db-after (d/transact old params)))})


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

  (om/root (partial app ["eve@polyc0l0r.net"
                         #uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                         "master"])
           (-> @stage :volatile :val-atom)
           {:target (. js/document (getElementById "core-app"))})

  #_(<! (s/connect! stage "ws://localhost:8080/geschichte/ws"))

  ;; some dummy state
  (add-watch (-> @stage :volatile :val-atom) :add-once
             (fn [k a o new]
               (when (get-in new ["eve@polyc0l0r.net"
                                  #uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                                  "master"])
                 (remove-watch a k)
                 (go (<! (s/transact stage
                                     ["eve@polyc0l0r.net"
                                      #uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4"
                                      "master"]
                                     [{:db/id (uuid)
                                       :lon 8.485 :lat 49.455
                                       :meal-type :soup
                                       :user "tom"
                                       :description "Kürbis-Chreme-Suppe"
                                       :start-ts #inst "2014-06-15T09:00:00.000-00:00"
                                       :end-ts #inst "2014-06-15T12:00:00.000-00:00"
                                       :ts (js/Date.)}
                                      {:db/id (uuid)
                                       :lon 8.491 :lat 49.451
                                       :meal-type :lunch
                                       :user "eve"
                                       :description "Pommes mit Bananensauce"
                                       :start-ts #inst "2014-06-15T11:00:00.000-00:00"
                                       :end-ts #inst "2014-06-15T16:00:00.000-00:00"
                                       :ts (js/Date.)}
                                      {:db/id (uuid)
                                       :lon 8.494 :lat 49.458
                                       :user "john"
                                       :description "Kirschkuchen"
                                       :meal-type :cake
                                       :start-ts #inst "2014-06-15T18:00:00.000-00:00"
                                       :end-ts #inst "2014-06-15T22:00:00.000-00:00"
                                       :ts (js/Date.)}]

                                     '(fn [old params]
                                        (:db-after (d/transact old params)))))
                     (<! (s/commit! stage
                                    {"eve@polyc0l0r.net"
                                     {#uuid "98bac5ab-7e88-45c2-93e6-831654b9bff4" #{"master"}}})))))))


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
