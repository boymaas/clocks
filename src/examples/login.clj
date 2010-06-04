(ns exampels.login
  (:use clocks.core)
  (:use hiccup.core))

(defn validate-email [email]
     "message")

(defpage index "/index"
  (html [:html
         [:head
          [:title "Welcome"]]
         [:body
          [:h1 "login"]
          (defblock :login-form []
            [:form {:method :post}
             (defblock :email [email]
               [:input {:type :text
                        :value email
                        :onchange (str "$(this).post(" (block-url :email-msg) r* ", {email: this.value})")}])
             (defblock :email-msg [email]
               (if-let [msg (validate-email email)]
                 [:div.errormsg msg]))])]]))
