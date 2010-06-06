(ns examples.login
  (:use compojure.core
        (ring.middleware session params reload keyword-params stacktrace file))
  (:require [compojure.route :as route])
  
  (:use ring.adapter.jetty)
  (:use clocks.core
        clocks.defjs
        clocks.jquery)
  (:use clojure.contrib.trace)
  (:use [clojure.contrib.duck-streams :only (append-spit)])
  (:use hiccup.core
        hiccup.page-helpers))

(defn validate-email [email]
  "message")

(defblock login-form [] 
  [:form {:id :login_form :method :post :url nil}

   (block login-form-fields [email password]
          [:input {:type :text, :id :login-form-email, :value email}]
          [:input {:type :password, :value password}])

   [:button {:id :login-form-submit}]] 

  [:div#login-form-messages]

  ($defjs
   (fn says [s]
     (alert (+ "Simon sais:" s)))

   ($id-on-event :login-form-email keyup
                 ($id-call :login-form-messages html
                           ($id-call :login-form-email val)))

   ($id-on-event :login-form-submit click 
                 ($id-reload :login-form-fields {:email (. Math random)})
                 (return false))))

(defroutes-page index "/index"
  [:html
   [:head
    (include-js "/jquery-1.4.2.min.js")]
   (block level1 [email] ;; todo check on vector
          [:h1 "Title" email]
          [:h2 (block-uri :level1)]

          (callblock login login-form)

          (block level2 []
                 [:p "paragraphs"]
                 [:ol (for [r routes*]
                        [:li r])]))])

(defroutes example
  index)

(defn wrap-debug-log [handler]
  (fn [r]
    (append-spit "debug.log" (format "%s?%s\n" (r :uri) (r :query-string)) )
    (handler r))) 

(wrap! example 
       (:debug-log)
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
