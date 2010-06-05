(ns exampels.login
  (:use compojure.core)
  (:require [compojure.route :as route])
  (:use ring.adapter.jetty)
  (:use clocks.core)
  (:use hiccup.core))

(defn validate-email [email]
     "message")

(defroutes-page index "/index"
  [:html
   (defblock level1 [email] ;; todo check on vector
     [:h1 "Title" email]
     (defblock level2 []
       [:p "paragraphs"]))])

(defroutes example
  index)

(defn thread [f]
  "creates a thread"
  (.start (Thread. f))) 

(defn start-server 
  "Starts server at a certain port, default is 8080"
  ([] (start-server 8080))
  ([port]
  (thread (fn [] (run-jetty (var example) {:port port})))))
