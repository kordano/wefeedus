(ns wefeedus.view
  (:require [figwheel.client :as fw :include-macros true]
            [kioo.om :refer [content set-attr do-> substitute listen prepend append html remove-class]]
            [kioo.core :refer [handle-wrapper]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [kioo.om :refer [defsnippet deftemplate]]))

(enable-console-print!)

(println "Resistance is futile!")

(fw/watch-and-reload
  ;; :websocket-url "ws://localhost:3449/figwheel-ws" default
 :jsload-callback (fn [] (print "reloaded"))) ;; optional callback


(defsnippet link-detail-comment "main.html" [:.link-detail-comment-item]
  [comment]
  {[:.link-detail-comment-text] (content (:text comment))
   [:.link-detail-comment-user] (content (:user comment))
   [:.link-detail-comment-timestamp] (content (:date comment))})


(defsnippet link-header-hashtag "main.html" [:.link-header-hashtag-item]
  [hashtag]
  {[:.link-header-hashtag-item-text] (content hashtag)})


(defsnippet link-detail "main.html" [:.link-detail]
  [record]
  {[:.link-detail] (set-attr "id" (str "link-item-" (:id record)))
   [:.link-detail-url] (do-> (set-attr "href" (:detail-url record))
                         (content (:detail-url record)))
   [:.link-detail-text] (content (:detail-text record))
   [:.link-detail-comment-list] (content (map link-detail-comment (:comments record)))})


(defsnippet link-header "main.html" [:.link-header]
  [record]
  {[:.link-header] (set-attr "href" (str "#link-item-" (:id record)))
   [:.link-header-text] (content (:title record))
   [:.link-header-user] (content (:user record))
   [:.link-header-hashtag-list] (content (map link-header-hashtag (:hashtags record)))})


(defsnippet link-item "main.html" [:.link-item]
  [record]
  {[:.link-comment-counter] (content (-> record :comments count))
   [:.link-header] (substitute (link-header record))
   [:.link-detail] (substitute (link-detail record))})


(deftemplate main-view "main.html" [data add-post]
  {[:.list-group] (substitute (map #(link-item %) data))
   [:#send-button] (listen :onClick add-post)})
