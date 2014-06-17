(ns wefeedus.view
  (:require [figwheel.client :as fw :include-macros true]
            [enfocus.core :as ef]
            [kioo.om :refer [content set-attr do-> substitute listen prepend append html remove-class]]
            [kioo.core :refer [handle-wrapper]]
            [cljs.core.async :refer [put!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [kioo.om :refer [defsnippet deftemplate]]
                   [enfocus.macros :as em]))

(enable-console-print!)

(println "Will plants grow?")

#_(fw/watch-and-reload
  ;; :websocket-url "ws://localhost:3449/figwheel-ws" default
 :jsload-callback (fn [] (print "reloaded"))) ;; optional callback


(em/deftemplate map-popup "popup.html"
  [user]
  ["#popup-user"] (ef/content (or user "foo")))

(deftemplate app-view "public/index.html" [map-comp start-ts-ch end-ts-ch date-ch]
  {[:#map] (substitute map-comp)
   [:#start-time] (listen :onChange (fn [e] (put! start-ts-ch (.. e -target -value))))
   [:#end-time] (listen :onChange (fn [e] (put! end-ts-ch (.. e -target -value))))
   [:#date] (listen :onChange (fn [e] (put! date-ch (.. e -target -value))))})
