(ns clocks.defjs 
  (:use clojure.walk)
  (:use hiccup.page-helpers)
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
  (prewalk (fn [x] (cond
                    (and (seq? x)
                         (symbol? (first x))
                         ;; need to get first character of name
                         ;; not of resolved var. clojure
                         ;; resolves names in a quasiquote 
                         ;; thus when used in another namespace we
                         ;; get problems when we don't de-resolve
                         (= (first (name (first x))) \$))
                     (macroexpand x)
                     ;; unresolve the probable resolved symbol
                     (symbol? x) (symbol (name x))
                     ;; default return
                     :default x))
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
  `(javascript-tag (js ~@(expand-js-macros body))))


(defmacro defjs-macro [name params & body]
  "define a javascript macro, javascript
macros are different from other macros since
they schould be evaluated inside a defjs block
they should just expand once. This is handled
in a defjs block.
"
  `(defmacro ~name ~params
    ~@(expand-js-macros body)))

