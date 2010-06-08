(ns clocks.defjs 
  (:use clojure.contrib.trace)
  (:use clojure.walk)
  (:use [com.reasonr.scriptjure :only (js)]))

(comment
  JAVASCRIPT MACROS

  since scriptures (js .. ) blocks macro expantion we need
  to expand our js-macro manually

  to do this we traverse the defined js-forms recursivel
  and keep on expanding untill all are expanded

  this is done by expand-js-macros 
  )

(defn expand-js-macros 
  "Recursively performs all possible macroexpansions in form."
  [form]
  (prewalk (fn [x] (if (and (seq? x)
                            (= (first (str (first x))) \$))
                     (macroexpand x)
                     x))
           form))

(defn keyword->cssid [k]
  (str "#" (subs (str k) 1)))

;; API

;; jQuery javascript wrapper
(defmacro defjs [& body]
  "wraps generated scripts in a jquery onloaded and
   script tag, we need to eval the seperate form
   for output otherwise macro-expantion of nested macro's
   stops."
  `[:script (js ~@(expand-js-macros body))])


(defmacro defjs-macro [name params & body]
  "define a javascript macro, javascript
macros are different from other macros since
they schould be evaluated inside a defjs block
they should just expand once. Thisis handled
in the defjs block.
"
  `(defmacro ~name ~params
    ~@(expand-js-macros body)))

