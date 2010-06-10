(ns clocks.core
  (:use 
        clojure.contrib.trace
        hiccup.core
        compojure.core
        clojure.walk
        clocks.data))
     
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
                                  `(~(sf->function-name prefix (msf name)) ~'uri-prefix* r*)
                                  ))
                              [] (:body sf))))))

;; WRAPPING

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

(defn- wrap-page
  "wraps on page level"
  [name params body]
  `(let [{:strs ~(vec params)} ~'p*]
     (html ~@body)))


(defn- sf->fn
  "closes route information inside an
anonymous function binding all the helper
variables"
  [sf]
  `(fn [~'uri-prefix* request#]
     (binding [routes* (prepend-uri-prefix ~'uri-prefix* ~'page-route-map*)]

       ~(let [{:keys [type name params body]} sf]
          (case type
                'page (wrap-page name params body)
                'block (wrap-block name params body)
                'json (wrap-json name params body)
                (throw (Exception. (str "Unknown type to wrap: " type) )))))))

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
  `(~'page ~name ~params ~@body))

(defn- sf-root?
  "checks if it is a root-block by its name"
  [sf]
  (= (:name sf) *sf-root-name*))

(defn- vsf->any-routes
  "generates any routes for a vsf"
  [vsf func-prefix url-prefix]
  (for [sf vsf]
    `(ANY ~(sf->abs-uri sf url-prefix) [] (wrap-request-bindings ~(sf->function-name func-prefix sf) ~url-prefix))))

(defn- unwrap-root-path [vsf]
  "since we wrap a block around a page-block
we have an extra level in our path which we
remove since we don't have to introduce cases in our
code to cope with this unneeded prefix"
  (map #(assoc % :path (rest (:path %))) vsf))

(defn- walk-symbol->str [t]
  "prewalker transforming all encountered
symbols to strings"
  (prewalk (fn [f] (if (symbol? f)
                     (str f)
                     f)) t))

(defn- vsf->mexpandable-vsf [vsf]
  "make vsf ready to be macroexpad, need to suse
  this is needed in defpage"
  (walk-symbol->str (map #(assoc % :body nil :path (vec (:path %))) vsf)))

;; API
(defmacro defpage
  "to define a page"
  [func-prefix params & body]
  (assert (symbol? func-prefix))
  (assert (vector? params))
  (let [vsf:block->vsf:fn-call (partial vsf:block->vsf:fn-call func-prefix)
        vsf (-> (wrap-root-block *sf-root-name* body params) 
                (body->expanded-body)     ;; expand callblocks
                (body->vsf)              ;; extract sf
                (unwrap-root-path)       ;; remove wrapped from path
                (vsf:block->vsf:fn-call) ;; implode blocks to funcalls
                )]
    `(do
       (let [~'page-route-map* ~(vsf->route-map vsf)]
         ;; generate functions for partials
         ~@(vsf->defn func-prefix vsf))

       ;; generate any routes for functions
       (def ~func-prefix {:vsf ~(vec (vsf->mexpandable-vsf vsf))
                          :func-prefix ~(str func-prefix)}))))

(defn wrap-request-bindings 
  "wraps around needed bindings"
  [handler prefix]
  (fn [request]
     (binding [r* request
               s* (:session request)
               p* (:params request)
               method* (:method request)]
       (handler prefix request))))

(defmacro PAGE [url-prefix page]
  (assert (string? url-prefix))
  (assert (symbol? page))
  (let [page (var-get (resolve page))]
    `(routes ~@(vsf->any-routes (:vsf page) (:func-prefix page) url-prefix))))

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

