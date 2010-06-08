(ns examples.login
  (:use compojure.core
        (ring.middleware session params reload keyword-params stacktrace file))
  (:require [compojure.route :as route])
  
  (:use ring.adapter.jetty)
  (:use clocks.core
        clocks.defjs
        clocks.jquery)
  (:use clojure.contrib.trace
        clojure.contrib.pprint)
  (:use [clojure.contrib.duck-streams :only (append-spit)])
  (:use hiccup.core
        hiccup.page-helpers))

(defn validate-email [email]
  "message")

(defblock login-form [] 
  (block little-one []
         "Nothing really interesting ..")

  [:form {:id :login_form :method :post :url nil}

   (block login-form-fields [email password]
          (block nested [blah]
                 "see if this makes a difference")
          
          [:input {:type :text, :id :login-form-email, :value email}]
          [:input {:type :password, :value password}])
   

   [:button {:id :login-form-submit}]] 


  [:div#login-form-messages]

  (block validate [email]
         [:h2 email]
         (if (not= email "boy")
           [:h3 "incorrect"]
           [:h3 "correct"]))

  (block validate-json [email]
         (str  (if (not= email "boy")
                 {:result true}
                 {:result false})))
  
  ;; javascript generation is sloooww ...
  ($defjs
   (fn says [s]
     (alert (+ "Simon sais:" s))))

  ($defjs

   ($id-on-event :login-form-email keyup
                 (var v this.value)
                 ($id-call :login-form-messages html
                           v))

   ($id-on-event :login-form-email keyup
                 ($id-reload :validate {:email ($id-value :login-form-email)}))

   ($id-on-event :login-form-submit click 
                 ($id-reload :login-form-fields {:email (. Math random)})
                 (return false))))

(defroutes-page index "index"
  [:html
   [:head
    (include-js "/jquery-1.4.2.min.js")]
   (block level1 [email] ;; todo check on vector
          [:h1 "Title" email]
          [:h2 (block-uri :level1)]

          (callblock :login-form login-form)

          (block level2 []
                 [:p "paragraphs"]
                 [:ol (for [[k v] routes*]
                        [:li k "-->" v])]))])

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
