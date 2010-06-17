(ns clocks.core
  (:use 
   clojure.contrib.trace
   clojure.contrib.json.write
   hiccup.core
   compojure.core
   compojure.response
   clojure.walk
   clocks.data
   clocks.md5))
(comment
  sf    spefical form eg. callblock block
  vsf   vector special form
  msf   map special form, indexed by name
  )

(def *sf-root-name* "root")

;; the to be bound variables
;; for usage in helper functions
(declare r* s* p* method* routes* in-page* func-name* func-uri*)

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
                              (format  "Trying to call a nonexistsent block: (%s)" block-id)
                              ;; expand to block by finding the element form
                              ;; the registry and expanding it so the code scanner can
                              ;; create all the routes etc ...
                              (special-form->block (assoc (*defblock-registry* block-id) :name (keyword->symbol label)))) 
                            ))
          ;; extra params
          [] body ))

;; extract all individual blocks
(declare *accumulator*)
(defn- body->vsf
  "extracts body blocks out of code tree"
  ([body] (body->vsf body []))
  ([body path]
     (binding [*accumulator* (atom  [])]
       (walker is-block? (fn [p b]
                           (swap! *accumulator* conj (block->special-form b p))
                           b)
               [] body)
       @*accumulator*)))


;; parse individual blocks and transform block
;; definition to named - callblocks calling to be generated functions

;; also strips json 
;; todo: rename to vsf:block->vsf:block-collapsed

(defn- vsf:block->vsf:fn-call 
  "replaces (body ..) blocks in every element of a vsf
to a direct function call"
  [prefix vsf]
  (let [msf (vsf->msf vsf)]
    (for [sf vsf]
      (assoc sf :body (walker is-block? (fn [p b]
                                          (let [[type name] b]
                                            (case type
                                                  ;; in case of json blocks
                                                  ;; these schouldn't be executed
                                                  'json nil
                                                  ;; rest call from function --> (funcall *r)
                                                  `(~(sf->function-name prefix (msf name)) ~'uri-prefix* r*))
                                            ))
                              [] (:body sf))))))

;; WRAPPING
(defn- wrap-block 
  "wraps a block into a form able
to output a html request and preparses
request params"
  [name params body]
  `(let [{:strs ~(vec params)} ~'p*]
     ;; [:div#id]
     [~(keyword (str "div#" (str name)))
      ~@body]))

(defn- wrap-block-on-post
  "wraps a block, will execute only on a post"
  [name params body]
  `(when (= (r* :request-method) :post)
     ~(wrap-block name params body)))

(defn- wrap-json
  "wraps a block into a form able
to output a json request and preparses
request params"
  [name params body]
  `(let [{:strs ~(vec params)} ~'p*]
     (json-str ~@body)))

(defn- wrap-page
  "wraps on page level"
  [name params body]
  ;; one element map
  ;; cannot nest hiccup html elements, see doc
  `(let [{:strs ~(vec params)} ~'p*]
     ~@body))


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
                'block-on-post (wrap-block-on-post name params body)
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

(defn- body->block
  "wrappes a root-block inside a block
enabling the tree-walker to include the root
definition"
  [type name body params]
  `(~(symbol type) ~name ~params ~@body))

(def wrap-root-block (partial body->block 'page))

(defn- vsf->any-routes
  "generates any routes for a vsf"
  [vsf func-prefix url-prefix]
  ;; build routes most specific first
  (for [sf (sort (fn [sfa sfb] (- (count (sfb :path))
                                  (count (sfa :path)))) vsf)]
    (let [func-name (sf->function-name func-prefix sf)
          func-uri (sf->abs-uri sf url-prefix)]
      `(ANY ~func-uri [] (wrap-request-bindings ~(:func-ref sf)      ;; partial function ref
                                                ~url-prefix          ;; url prefix
                                                ~(str func-name)     ;; for block self-awareness
                                                ~(str func-uri)))))) ;; for block self-awareness

(defn- unwrap-root-path
  "since we wrap a block around a page-block
we have an extra level in our path which we
remove since we don't have to introduce cases in our
code to cope with this unneeded prefix"
  [vsf]
  (map #(assoc % :path (rest (:path %))) vsf))

(defn- walk-symbol->str
  "prewalker transforming all encountered
symbols to strings"
  [t]
  (prewalk (fn [f] (if (symbol? f)
                     (str f)
                     f)) t))

(defn- vsf->mexpandable-vsf 
  "make vsf ready to be macroexpanded, need to suse
  this is needed in defpage"
  [vsf func-prefix]
  (walk-symbol->str (map #(assoc % :body nil
                                 :path (vec (:path %))
                                 ;; create funcref since (PAGE ..) could
                                 ;; be used in another namespace
                                 :func-ref (sf->function-name func-prefix %)) vsf)))


;; selective expantions
;; todo make this generic, expand excluding specific forms ..

(defn do-not-expand? [x]
  (and (seq? x)
       ;; do not expand html
       (symbol? (first x))
       ;; in symbol table or a javascript macro (beginning with $
       (or  (some #(= (symbol (name (first x))) %) ['block 'html 'js 'fn 'when 'when-not 'if])
            (= (first (name (first x))) \$))))

(defn macroexpand-except-blocks
  "Repeatedly calls macroexpand-1 on form until it no longer
  represents a macro form, or encounters a do-not-expand."
  {:added "1.0"}
  [form]
    (let [ex (macroexpand-1 form)]
      (if (or (do-not-expand? ex)
              (identical? ex form))
        form
        (macroexpand-except-blocks ex))))

;; API
(defn macroexpand-all-except-blocks
  "Recursively performs all possible macroexpansions in form."
  {:added "1.1"}
  [form]
  (prewalk (fn [x] (cond
                    ;; if one of the special do not expand
                    (do-not-expand? x)
                    x
                    ;; not a special form but a sequence
                    ;; try to expand
                    (seq? x) (macroexpand-except-blocks x)
                    ;; the rest just return
                    :default x)) form))


(defmacro defpage
  "to define a page"
  [func-prefix params & body]
  (assert (symbol? func-prefix))
  (assert (vector? params))
  (assert (= (count body) 1))
  (let [body (macroexpand-all-except-blocks body)
        vsf:block->vsf:fn-call (partial vsf:block->vsf:fn-call func-prefix)
        vsf (-> (wrap-root-block *sf-root-name* body params) 
                (body->expanded-body)     ;; expand callblocks
                (body->vsf)             ;; extract sf
                (unwrap-root-path)      ;; remove wrapped from path
                (vsf:block->vsf:fn-call) ;; implode blocks to funcalls
                )]
    `(do
       (let [~'page-route-map* ~(vsf->route-map vsf)]
         ;; generate functions for partials
         ~@(vsf->defn func-prefix vsf))

       ;; generate any routes for functions
       (let [expandable-vsf# ~(vec (vsf->mexpandable-vsf vsf func-prefix))]

       ;; resolve the functionnames here, at compile time
       ;; making sure the defn's are created and *ns* is set
       ;; to the defpage caller
         (def ~func-prefix (fn [] {:vsf (for [sf# expandable-vsf#]
                                          (assoc sf# :func-ref (ns-resolve ~*ns* (symbol (:func-ref sf#)))))
                                   :func-prefix ~(str func-prefix)})))

       )))

(defn wrap-request-bindings 
  "wraps around needed bindings"
  ([handler prefix]
     (wrap-request-bindings handler prefix "*unknown*" "*unknown*"))
  ([handler prefix func-name func-uri]
      (fn [request]
        (binding [r* request
                  s* (atom (or (:session request) {}))
                  p* (:params request)
                  func-name* func-name
                  func-uri* func-uri
                  method* (:method request)]
          ;; using compojures.response/render
          (let [response (render {}  (html (handler prefix request)))]
            ;; setting updated or not session in repsonse
            ;; so ring handler can update
            (assoc response :session @s*))))))

(defmacro PAGE
  "Connects the clocks world to compojures world.
  (defroutes (PAGE uri page-def)) will generate
  any routes of all defined blocks"
  [url-prefix page]
  (assert (string? url-prefix))
  (assert (symbol? page))
  (let [page (var-get (resolve page))]
    (when-not (fn? page)
      (throw (Exception. "Supplied PAGE parameter does not resolve to correct type")))
    (let [config (page)]
      `(routes ~@(vsf->any-routes (:vsf config) (:func-prefix config) url-prefix)))))

(defmacro defblock
  "to define a block which can get expanded in a
defroutes-page, name will be stored in *defblock-registry* for the expander"
  [func-prefix params & body]
  (assert (symbol? func-prefix))
  (assert (vector? params))
  (let [wrapped-body (body->block 'block func-prefix body params)]
    (alter-var-root #'*defblock-registry* #(assoc % (keyword func-prefix) (block->special-form wrapped-body [])))
    nil))

;; finds uri for block
(defn clocks-uri
  "helper function to find the uri by block name"
  [block-name]
  (assert (keyword? block-name))
 (routes* block-name))

(defn clocks-uri-this
  "returns the uri of containing block"
  []
  func-uri*)

;; sesison hellpers
(defn clocks-session-put! [k v]
  (swap! s* (fn [a b] (merge a {k b})) v))

(defn clocks-session-get
  ([k] (clocks-session-get k nil))
  ([k default] (if (vector? k)
                 (get-in @s* k)
                 (get @s* k default))))

;; functions to ease testing
;; TODO: move to seperate ns
(defn callpartial
  "calls a partial"
  [url-prefix name request]
  ;; try to resolve name
  (let [name (or (resolve name)
                 (resolve (symbol (str  "clpartial-" name))))])
  `(let [response# ((wrap-request-bindings
                     ~name ~url-prefix) ;; name of partial
                    ~request)]
     (assoc response# :body-md5 (subs (md5-sum (response# :body)) 0 4)))) ;; in request format

(defn md5= [md5:0-4 response]
     (= md5:0-4 (:body-md5 response)))

(defmacro cp-params
  "Utility function for calling partial with params
   params can be specified as keywords they will be 
   converted to strings"
  [up n & p]
  (callpartial up n {:params (stringify-keys (apply hash-map p))}))

;; debugging

(defn clocks-debug-routes []
  [:ol (for [[n r] routes*]
         [:li n "-->" r])])
