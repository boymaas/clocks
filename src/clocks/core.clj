(ns clocks.core
  (:use (clojure.contrib ns-utils
                         ;; seq-utils
                         str-utils
                         )
        clojure.walk
        clojure.contrib.pprint
        clojure.contrib.trace
        hiccup.core
        compojure.core)
  (:require [clojure.zip :as zip]))
     
;; the to be bound variables
(declare r* s* p* method* routes* in-page*)

(def *defblock-registry* {})

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

(defn block-name [b]
  (let [[_ name & _] b]
    name))

(defn- block->special-form [b path]
  (let [[type name params & body] b]
    (struct special-form type name params body path)))

(defn- special-form->block [sf]
  (let [{:keys [type name params body]} sf]
    `(~type ~name ~params
            ~@body)))


;; recursive walker

(defn walker 
 " a depth first tree walker
 customized for blocks takes

f: funciton to be called to modify tree
   takes path and b shoud return value
   to update tree with 

path: initial path to start walking with

b: the body to parse
"
 ([f path b] (walker is-block? f path b))
 ([filter? f path b]
     (let [path (if (filter? b)
                  (conj path (block-name b))
                  path)]
       ;; define new function
       (letfn [(inner [b] (walker filter? f path b))]
         (prn path (filter? b) (subs (str b) 0 (min (count (str b)) 100) ))
         (let [b* (cond
                   (list? b) (apply list (map inner b))
                   (seq? b) (doall (map inner b))
                   (vector? b) (vec (map inner b))
                   ;;(map? b) (into (if (sorted? b) (sorted-map) {})
                   ;;                    (map inner b))
                   ;;(set? b) (into (if (sorted? b) (sorted-set) #{})
                   ;;                   (map inner b))
                   :else b)]

           ;; do something here with body
           (if (filter? b*)
             (f path b*)
             b*))))))


;; expand callblocks
(defn body->expanded-body [body]
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
(defn body->vsf-block
  ([body] (body->vsf-block body []))
  ([body path]
      (binding [*accumulator* []]
        (walker is-block? (fn [p b]
                  (set! *accumulator* (conj *accumulator*
                                            (block->special-form b p)))
                  b)
                [] body)
        *accumulator*)))

(defn vsf->msf [vsf]
  (into {} (map #(vector (:name %) %) vsf)))

(defn path->function-name [prefix p]
  (symbol (str-join "-" (concat ["clpartial" prefix] p))))

(defn- sf->function-name [prefix sf]
  (path->function-name prefix (:path sf)))

;; parse individual blocks and transform block
;; definition to named - callblocks calling to be generated functions
(defn vsf-block->vsf-callblock [prefix vsf]
  (let [route-map (vsf->msf vsf)]
    (for [sf vsf]
      (assoc sf :body (walker (fn [p b]
                                (let [[_ name] b]
                                  ;; (funcall *r)
                                  `(~(path->function-name prefix (:path (route-map name))) r*)
                                  ))
                              [] (:body sf))))))



(defn body->vsf-callblock [prefix body]
  ;; let all defined blocks call eachother
  (vsf-block->vsf-callblock prefix
                            ;; accumulate all special forms
                            (body->vsf-block
                             ;; expand all special forms that need
                             ;; expansion
                             (body->expanded-body body))))

;; WRAPPING

;; todo: wraps have common code
(defn wrap-block [name params body]
  `(html
    [~(keyword (str "div#" (str name)))
     (let [{:strs ~(vec params)} ~'p*]
       (html ~@body))]))

(defn wrap-json [name params body]
  `(let [{:strs ~(vec params)} ~'p*]
    ~@body))

;; wraps extracted block in default bound variables
;; creates shortcuts for request session params and t*
(defn sf->fn [sf]
  ;; clojure on defined routes information
  `(fn [request#]
     (let [routes# ~'routes*] ;; create local clojure for routing info
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

(defn- path->abs-uri [prefix p]
  (str "/" (str-join "/" (cons prefix p))))

(defn- sf->abs-uri [prefix sf]
  (path->abs-uri prefix (:path sf)))

(defn- vsf->route-map [prefix vsf]
  (into {} (map
            (fn [sf] [(keyword (:name sf)) (path->abs-uri prefix (:path sf))])
            vsf)))

;; macro expand to individual functions
;; named by prefix - path - name 
(defn sf->defn [prefix sf route-map]
  `(def ~(path->function-name prefix (:path sf))
            (let [~'routes* ~route-map]
              ~(sf->fn sf))))

(defn vsf->defn [prefix vsf]
  (let [route-map (vsf->route-map prefix vsf)]
    (for [sf vsf]
      (sf->defn prefix sf route-map))))

;; API


(defmacro block [name params & body]
  (wrap-block name params body))

(defmacro json [name params & body]
  (wrap-block name params body))


(defmacro defjson [name params & body]
  `(def ~name (struct special-form 'json  nil '~params '~body nil)))

;; defines on root level a set of routes
;; accessing the different blocks inside a page

;(defmacro defroutes-page [name prefix & body]
;  `(do ~@(define-block-functions name prefix body)))

(defn- wrap-root-block [name body params]
  `(~'block ~name ~params ~@body))


(defn sf-root? [sf]
  (= (:name sf) "$ROOT$"))

(defn- vsf->any-routes [vsf func-prefix url-prefix]
  `(apply routes
          [~@(for [sf vsf]
               `(ANY ~(if (sf-root? sf)
                        (path->abs-uri url-prefix [])
                        (sf->abs-uri url-prefix sf)) [] ~(sf->function-name func-prefix sf)))]))

(defmacro defroutes-page [func-prefix url-prefix & body]
  (let [vsf (body->vsf-callblock func-prefix (wrap-root-block "$ROOT$" body []))]
    `(do
       ;; generate functions for partials
       ~@(vsf->defn func-prefix vsf)

       ;; generate any routes for functions
       (def ~func-prefix
            ~(vsf->any-routes vsf func-prefix url-prefix))))
  )

(defmacro defblock [func-prefix params & body]
  (assert (symbol? func-prefix))
  (assert (vector? params))
  (let [wrapped-body (wrap-root-block func-prefix body params)]
    (alter-var-root #'*defblock-registry* #(assoc % (keyword func-prefix) (block->special-form wrapped-body [])))
    nil))


(comment xdrp []
  (defblock testblock [email password]
    [:h2 "yepperdepep"])

  (pprint (macroexpand '(defblock index []
                          [:html
                           [:head
                            (include-js "/jquery-1.4.2.min.js")]
                           (block level1 [email] ;; todo check on vector
                                  [:h1 "Title" email]
                                  [:h2 (block-uri :level1)]

                                  ;;(callblock login login-form)

                                  (block level2 []
                                         [:p "paragraphs"]
                                         [:ol (for [r routes*]
                                                [:li r])]))])))

  (pprint (body->expanded-body '(callblock :testblock blah)))
  )

(comment xdrp)

;; finds uri for block
(defn block-uri [block-name]
  (routes* block-name))

(comment

  CLOCKS.CORE DEFINES A WEBDSL ON TOP OF COMPOJURE.

  'defroutes-page' can be used to generate a page in which
  blocks can be accessed independitly from the rest of the system.

  This is implemented by scanning the source for special forms.

  'block' indicates a piece of code which can be accessed via a seperate route.
  these routes are defined by their path in the tree seperated by dots

  (defroutes-page name prefix
    (block level1 []
           ..code...
           (blcok level2 []
                  ..code..
                  (block level3 []
                         ...code..))))
  
  In this case level3 can be reached by

  prefix.level1.level2.level3. It will only render the ..code... part of block3.

  REUSABLE BLOCKS
  
  Reusable blocks can be defined by `defblock'. These can be called with `callblock' from whithin
  a page.
  
  (defblock name [params] ...)

  These are called from withing a page using 

  (callblock name predefined-block)

  EVALUATION CONTEXT / BINDINGS
  
  Blocks get transformed to anonymous functions with the following bindings, all with a * postfix 
  
  (binding [~'r* request#                     ;; request
            ~'s* (:session request#)          ;; session if available
            ~'p* (:params request#)           ;; params
            ~'routes* routes#                 ;; routes inside page
            ~'method* (:method request#)]     ;; method of request

    ...block code... )

  These bindings are present for helper functions. Such as `block-uri' which looks up
  the uri of the block by name.

  COMPILATION OF BLOCKS

  1. extract special forms & register routes
     block
     callblock

     and store them into array of block-routes. Storing name, paramsm path, unevaluated body. 


     the last block is the complete page

     callblocks get expanded into the defblocks which defined them.

 2. accumulate data from all defined forms
     build routes map _name _path from array of block routes
     routes can now be closed over in the individual blocks.
     
     3. wrap each individual block body in a anonymous function
     binding the accumulated data and request bindings (see bindings above)

     4. use these wrapped body's to build compojure ANY routes
     
     Keep as much flexibility in your data!
)
