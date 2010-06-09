(ns clocks.core
  (:use (clojure.contrib ns-utils
                         ;; seq-utils
                         str-utils
                         )
        clojure.walk
        clojure.contrib.fcase
        hiccup.core
        compojure.core)
  (:require [clojure.zip :as zip]))
     
(comment
  sf    spefical form eg. callblock block
  vsf   vector special form
  msf   map special form, indexed by name
  )

(def *sf-root-name* "root")

;; the to be bound variables
;; for usage in helper functions
(declare r* s* p* method* routes* in-page*)

;; registry for defined blocks
;; defined blocks will get expaned inside
;; a page definition
(def *defblock-registry* {})

;; struct storing information on a special
;; form
(defstruct special-form :type :name :params :body :path)

;; some helpers
(defn- symbol-and-eq? [s name]
  (and (seq? s)
       (symbol? (first  s))
       ;; symbols resolve to same namespace var
       (= (first s) name)))

(defn- is-block? [s]
  (symbol-and-eq? s 'block))

(defn- is-callblock? [s]
  (symbol-and-eq? s 'callblock))

(defn- block-name [b]
  (second b))

(defn- block->special-form 
  "converts block to special form"
  [b path]
  (let [[type name params & body] b]
    (struct special-form type name params body path)))

(defn- special-form->block
  "converts special block back to special form" 
  [sf]
  (let [{:keys [type name params body]} sf]
    `(~type ~name ~params
            ~@body)))

;; recursive walker
(defn- walker 
  "depth-first tree walker replacing walked node
    with return value of provided function"
  [filter? f path b]
  (let [path (if (filter? b)
               (conj path (block-name b))
               path)
        walker* (partial walker filter? f path)]
    ;; define new function
       
    (let [b* (cond
              (list? b) (apply list (map walker* b))
              (seq? b) (doall (map walker* b))
              (vector? b) (vec (map walker* b))
              :else b)]

      ;; do something here with body
      (if (filter? b*)
        (f path b*)
        b*))))

;; expand callblocks
(defn- body->expanded-body
  "expands callblocks inside body"
  [body]
  (walker is-callblock? (fn [p b]
            (let [[type block-id label] b]
              ;; find name in registry
              ;; expand form
              (if (not (contains? *defblock-registry* block-id))
                (format  "Trying to call a nonexistsent body: (%s)" block-id)
                ;; expand to block by finding the element form
                ;; the registry and expanding it so the code scanner can
                ;; create all the routes etc ...
                (special-form->block (assoc (*defblock-registry* block-id) :name label))) 
              ))
          ;; extra params
          [] body ))

;; extract all individual blocks
(declare *accumulator*)
(defn- body->vsf
  "extracts body blocks out of code tree"
  ([body] (body->vsf body []))
  ([body path]
      (binding [*accumulator* []]
        (walker is-block? (fn [p b]
                  (set! *accumulator* (conj *accumulator*
                                            (block->special-form b p)))
                  b)
                [] body)
        *accumulator*)))

(defn- vsf->msf
  "converts vector of special forms to map
indexed by name"
  [vsf]
  (into {} (map #(vector (:name %) %) vsf)))

(defn- path->function-name
  "converts a path [] to a function name always prefixed
with and identifier to prevent namespace collisions"
  [prefix p]
  (symbol (str-join "-" (concat ["clpartial" prefix] p))))

(defn- sf->function-name
  "see path->function-name"
  [prefix sf]
  (path->function-name prefix (:path sf)))

;; parse individual blocks and transform block
;; definition to named - callblocks calling to be generated functions
(defn- vsf:block->vsf:fn-call 
  "replaces (body ..) blocks in every element of a vsf
to a direct function call"
  [prefix vsf]
  (let [msf (vsf->msf vsf)]
    (for [sf vsf]
      (assoc sf :body (walker is-block? (fn [p b]
                                (let [[_ name] b]
                                  ;; (funcall *r)
                                  `(~(sf->function-name prefix (msf name)) r*)
                                  ))
                              [] (:body sf))))))



(defn- body->vsf:fn-call 
  "does a complete transformation of body
to vsf to be used in final macroexpantion"
  [prefix body]
  (vsf:block->vsf:fn-call prefix
                            ;; accumulate all special forms
                            (body->vsf
                             ;; expand all special forms that need
                             ;; expansion
                             (body->expanded-body body))))

;; WRAPPING

;; todo: wraps have common code
(defn- wrap-block 
  "wraps a block into a form able
to output a html request and preparses
request params"
  [name params body]
  `(html
    [~(keyword (str "div#" (str name)))
     (let [{:strs ~(vec params)} ~'p*]
       (html ~@body))]))

(defn- wrap-json
  "wraps a block into a form able
to output a json request and preparses
request params"
  [name params body]
  `(let [{:strs ~(vec params)} ~'p*]
    ~@body))

(defn- sf->fn
  "closes route information inside an
anonymous function binding all the helper
variables"
  [sf]
  `(fn [request#]
     (let [routes# routes*] ;; create local clojure for routing info
       (binding [r* request#
                 s* (:session request#)
                 p* (:params request#)
                 routes* routes# 
                 method* (:method request#)]

         ~(let [{:keys [type name params body]} sf]
             (case type
                   'json (wrap-json name params body)
                   'block (wrap-block name params body)
                   (throw (Exception. (str "Unknown type to wrap: " type) ))))))))

(defn- path->abs-uri
  "see sf->abs-uri .."
  [prefix p]
  (str "/" (str-join "." (cons prefix p))))

(defn- sf->abs-uri
  "returns a path to the special form"
  [prefix sf]
  (path->abs-uri prefix (:path sf)))

(defn- vsf->route-map
  "transforms vsf to a route-map indexed by block name"
  [prefix vsf]
  (into {} (map
            (fn [sf] [(keyword (:name sf)) (sf->abs-uri prefix sf)])
            vsf)))

;; macro expand to individual functions
;; named by prefix - path - name 
(defn- sf->defn
  "transforms sf to defn"
  [prefix sf]
  `(def ~(sf->function-name prefix sf)
        ~(sf->fn sf)))

(defn- vsf->defn
  "returns a vector of defn's from a vsf"
  [prefix vsf]
  (for [sf vsf]
    (sf->defn prefix sf)))

(defn- wrap-root-block
  "wrappes a root-block inside a block
enabling the tree-walker to include the root
definition"
  [name body params]
  `(~'block ~name ~params ~@body))

;; TODO: fix having to name the root-block with
;; a special extention .. ugly and prone to name collision
(defn- sf-root?
  "checks if it is a root-block by its name"
  [sf]
  (= (:name sf) *sf-root-name*))

(defn- vsf->any-routes
  "generates any routes for a vsf"
  [vsf func-prefix url-prefix]
  `(apply routes
          [~@(for [sf vsf]
               `(ANY ~(sf->abs-uri url-prefix sf) [] ~(sf->function-name func-prefix sf)))]))

(defn- unwrap-root-path [vsf]
  "since we wrap a block around a page-block
we have an extra level in our path which we
remove since we don't have to introduce cases in our
code to cope with this unneeded prefix"
  (map #(assoc % :path (rest (:path %))) vsf))

;; API
(defmacro defroutes-page
  "to define a page"
  [func-prefix url-prefix & body]
  (assert (symbol? func-prefix))
  (assert (string? url-prefix))
  (assert (sequential? body))
  (let [vsf:block->vsf:fn-call (partial vsf:block->vsf:fn-call func-prefix)
        vsf (-> (wrap-root-block *sf-root-name* body []) 
                (body->expanded-body)     ;; expand callblocks
                 (body->vsf)              ;; extract sf
                 (unwrap-root-path)       ;; remove wrapped from path
                 (vsf:block->vsf:fn-call) ;; implode blocks to funcalls
                 )]
    `(do
      (let [~'routes* ~(vsf->route-map url-prefix vsf)]
       ;; generate functions for partials
       ~@(vsf->defn func-prefix vsf))

      ;; generate any routes for functions
      (def ~func-prefix
           ~(vsf->any-routes vsf func-prefix url-prefix)))))

(defmacro defblock
  "to define a block which can get expanded in a
defroutes-page, name will be stored in *defblock-registry* for the expander"
  [func-prefix params & body]
  (assert (symbol? func-prefix))
  (assert (vector? params))
  (let [wrapped-body (wrap-root-block func-prefix body params)]
    (alter-var-root #'*defblock-registry* #(assoc % (keyword func-prefix) (block->special-form wrapped-body [])))
    nil))

;; finds uri for block
(defn block-uri
  "helper function to find the uri by block name"
  [block-name]
  (assert (keyword? block-name))
 (routes* block-name))

