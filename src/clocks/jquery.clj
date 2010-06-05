(ns clocks.jquery 
  (use clocks.core)
  (use clocks.defjs)
  (use [com.reasonr.scriptjure :only (js)]))

(defmacro $defjs [& body]
  "wraps generated scripts in a jquery onloaded and
   script tag, we need to eval the seperate form
   for output otherwise macro-expantion of nested macro's
   stops."
  `[:script (js ($ (fn [] ~@(render-js-forms body))))])

;; jquery helper macros to speed up jquery development

(defjs-macro $. [id path & body]
`(~'. (~'$ ~(keyword->cssid id)) ~path (fn [] ~@body)))

(comment TESTS

 (defjs
   ($. :testbutton click
       (alert "blah"))
   (alert "Hello"))

 )
