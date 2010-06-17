(ns clocks.jquery 
  (use clocks.core)
  (use clocks.defjs)
  (use [hiccup.core :only (html)])
  (use [clojure.contrib.except :only (throwf)])
  (use [com.reasonr.scriptjure :only (js)]))

(comment
  Jquery specific convenience macros, seperated from
  defjs since it couples to clocks.core)

(declare $)

(defmacro $defjs [& body]
  "wraps generated scripts in a jquery onloaded and
   script tag, we need to eval the seperate form
   for output otherwise macro-expantion of nested macro's
   stops."
  `(defjs ($ (fn [] ~@(expand-js-macros body)))))

;; jquery helper macros to speed up jquery de velopment

;; quasiquote always resolves symbols used so either
;; escape them or use lists

(defjs-macro $call [xpath func & params]
  `(. ($ ~xpath) ~func ~@params))

(defjs-macro $value [xpath]
  `($call ~xpath ~'val))

(defjs-macro $set-value [xpath value]
  `($call ~xpath ~'val ~value))

(defjs-macro $html [xpath htmlcode]
  `($call ~xpath html ~htmlcode))

(defjs-macro $html-hiccup [xpath & hiccup-html]
  `($html ~xpath ~(html hiccup-html)))

(defjs-macro $id-on-event [id name & body]
  `(. ($ ~(keyword->cssid id)) ~name (fn [~'ev] ~@body)))

(defjs-macro $id-call [id func & params]
  `(. ($ ~(keyword->cssid id)) ~func ~@params))

(defjs-macro $id-value [id]
  `($id-call ~id ~'val))

(defjs-macro $id-reload [id & params]
  `($id-call ~id ~'load (clj (clocks-uri ~id)) ~@params))

(defjs-macro $id-override-submit [id & body]
  `($id-on-event ~id submit
                ~@body
                (return false)))


(defn get-form-values-by-fieldnames [cssid field-names]
  (into {} (map #(vector (keyword %) `($value ~(str "form" (keyword->cssid cssid) " input[name=" % "]")))
                 field-names)))

;; todo: make parameter names better

;; posts parameters to block
(defjs-macro $id-reload-form-submit [block-id form-css-id & params]
  `($id-reload ~block-id ~(get-form-values-by-fieldnames form-css-id params)))

(defjs-macro $id-ajax-post->json [clocks-id params & keywords]
  (let [{:keys [on-success on-error]} keywords]
    `($.post (clj (clocks-uri ~clocks-id)) ~params ~on-success "json")))

;; high level macro submitting certain form fields to a
;; predifined block and executingthe on-success javascript
;; on success
;; id = id of form
;; target = target block
;; field-names = are the field names
;; on-success = on success function
(defjs-macro $id-form-submit [id target field-names & on-success]
  (when (some #(.contains (str %) "-") field-names)
    (throwf "'-' is not allowed in javascript symbols"))
   `($id-override-submit
    ~id
    ($id-ajax-post->json
     ~target
     ~(into {}
            (map #(vector (keyword %) `($value ~(str "form" (keyword->cssid id) " input[name=" % "]")))
                 field-names))
     :on-success ~@on-success)))
