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
  [user description]
  ["#popup-user"] (ef/content user)
  ["#popup-description"] (ef/content description))

(deftemplate app-view "public/index.html" [map-comp]
  {[:#map] (substitute map-comp)
   [:#start-time] (listen :onChange (fn [e] (.log js/console "kioo changed date:" (.-date e))))
   [:#end-time] (listen :onChange (fn [e] (.log js/console "kioo changed date:" (.-date e))))
   [:#date] (listen :onChange (fn [e] (.log js/console "kioo changed date:" (.-date e))))})
