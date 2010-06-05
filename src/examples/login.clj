(ns exampels.login
  (:use compojure.core
        (ring.middleware session params reload keyword-params stacktrace file))
  (:require [compojure.route :as route])
  
  (:use ring.adapter.jetty)
  (:use clocks.core
        clocks.defjs
        clocks.jquery)
  (:use clojure.contrib.trace)
  (:use hiccup.core
        hiccup.page-helpers))

(defn validate-email [email]
  "message")

(defroutes-page index "/index"
  [:html
   [:head
     (include-js "/jquery-1.4.2.min.js")]
      (defblock level1 [email] ;; todo check on vector
        [:h1 "Title" email]
        [:h2 (block-uri :level1)]
        [:input {:type :button :id :testbutton}]
        ($defjs
          ($. :testbutton click
              (alert "blah"))
          (alert "Hello world"))
        
        (defblock level2 []
          [:p "paragraphs"]
          [:ol (for [r routes*]
                 [:li r])]))])

(defroutes example
  index)

(wrap! example 
       (:session)
       (:file "public/"))

(defn thread [f]
  "creates a thread"
  (.start (Thread. f))) 

(defn start-server 
  "Starts server at a certain port, default is 8080"
  ([] (start-server 8080))
  ([port]
  (thread (fn [] (run-jetty (var example) {:port port})))))
