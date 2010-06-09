(ns examples.login
  (:require [compojure.route :as route])
  (:use compojure.core
        (ring.middleware session file)
        ring.adapter.jetty
        clocks.core
        clocks.defjs
        clocks.jquery
        [clojure.contrib.duck-streams :only (append-spit)]
        hiccup.core
        hiccup.page-helpers))

(defblock login-form [] 
  [:form {:id :login_form :method :post :url nil}

   (block login-form-fields [email password]
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
  
  ($defjs

   ;; echo typed messages to div#login-form-messages
   ($id-on-event :login-form-email keyup
                 (var v this.value)
                 ($id-call :login-form-messages html
                           v))

   ;; do some server-isde validation
   ($id-on-event :login-form-email keyup
                 ($id-reload :validate {:email ($id-value :login-form-email)}))

   ;; post a request via jquery to email
   ($id-on-event :login-form-submit click 
                 ($id-reload :login-form-fields {:email (. Math random)})
                 (return false))))

(defpage testpage 
  [:html
   [:head]])

(defpage index
  [:html
   [:head
    (include-js "/jquery-1.4.2.min.js")]
   (block level1 [] ;; todo check on vector
          [:h1 "Clocks example"]
          [:h2 (block-uri :level1)]

          ;; expands :login-form defined block here ..
          (callblock :login-form login-form)

          ;; print defined routes in this page
          (block level2 []
                 [:ol (for [[k v] routes*]
                        [:li k "-->" [:a.pol {:href v} v]])]))

   (block page-output []
          "page output")]

  ;; do some jquery to load different blocks inside
  ;; the page-output block
  ($defjs
   (. ($ "a.pol") click (fn [event]
                          (var url this.href)
                          (. ($ "#page-output") load url)
                          (. event preventDefault)
                          (return false)))))



;; define example servlet
(defroutes example
  (PAGE "/index" index))

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
