(ns clocks.defjs 
  (:use clojure.contrib.trace)
  (:use [com.reasonr.scriptjure :only (js)]))

(comment
  JAVASCRIPT MACROS

  since (js .. ) blocks macro
  expantion we need to expand manually and feed the expantion
  into a js functions


  we need js-helpers to check if its a helper and should
  be expanded. Took some time to figure out..
  )

;; using a set since contains? does not
;; traverse any indexed collections and
;; we need to compare 
(def *js-helpers* #{})

;; js-helpers
(deftrace render-js-forms [js-forms]
  (reduce
   (fn [a s]
     (conj a (if (not (sequential? s))
               s ;; where finished here
               ;; when sequential examine if its a helper
               (if (contains? *js-helpers* (first s))
                 (macroexpand-1 (render-js-forms s)) ;; expand
                 (if (vector? s)
                   (apply vector (render-js-forms s))
                   (render-js-forms s)
                   )) ;; if not dive in for the rest 
               )))
   '()
   (reverse js-forms)))

(defn keyword->cssid [k]
  (str "#" (subs (str k) 1)))

;; API

;; jQuery javascript wrapper
(defmacro defjs [& body]
  "wraps generated scripts in a jquery onloaded and
   script tag, we need to eval the seperate form
   for output otherwise macro-expantion of nested macro's
   stops."
  `[:script (js ~@(render-js-forms body))])


(defmacro defjs-macro [name params & body]
  "define a javascript macro, javascript
macros are different from other macros since
they schould be evaluated inside a defjs block
they should just expand once. Thisis handled
in the defjs block.
"
  (do
    (alter-var-root #'*js-helpers* conj name)
    `(defmacro ~name ~params
       ~@(render-js-forms body))))




