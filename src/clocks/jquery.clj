(ns clocks.jquery 
  (use clocks.core)
  (use clocks.defjs)
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
  `[:script (js ($ (fn [] ~@(expand-js-macros body))))])

;; jquery helper macros to speed up jquery de velopment

;; quasiquote always resolves symbols used so either
;; escape them or use lists

(defjs-macro $call [xpath func & params]
  `(. ($ ~xpath) ~func ~@params))

(defjs-macro $id-on-event [id name & body]
  `(. ($ ~(keyword->cssid id)) ~name (fn [~'ev] ~@body)))

(defjs-macro $id-call [id func & params]
  `(. ($ ~(keyword->cssid id)) ~func ~@params))

(defjs-macro $id-value [id]
  `($id-call ~id ~'val))

(defjs-macro $id-reload [id & params]
  `($id-call ~id ~'load (clj (clocks-uri ~id)) ~@params))

