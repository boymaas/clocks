;; functionality to define blocks inside a place and
;; generate small subfunctions to render them individually
;;
;; couples to compojure routes
;;
;; important exports:
;;
;; defroutes-page [name prefix & body]
;;   converts page definitions into
;;   small subfunctions callable via the defined
;;   routes
;;
;; defblock [name param & block]
;;   defines a block, returnin a string
;;   renderable individually and inside a page

(ns clocks.core
  (:use (clojure.contrib ns-utils
                         ;; seq-utils
                         str-utils
                         trace)
        hiccup.core
        compojure.core))

;; the to be bound variables
(declare r* s* p* method* routes* in-page*)

;; extracting defblock from declaration
;; and associating these blocks with a specific
;; route
(defstruct block-route :name :params :body :path)
(defn- extract-block-routes [body path]
  "parses defblocks recursively
   returning tree with route"
  (flatten
   (for [s (filter sequential? body)]
     ;; quicly macro-callblocks so routes and rest
     ;; can be extracted
     (let [s (if (=  (first s) 'callblock)
               (trace (macroexpand-1 s))
               s)]
       (if (and (symbol? (first  s))
                ;; symbols resolve to same namespace var
                (= (resolve (first s)) (resolve 'block)))
         (let [[_ name & body] s
               path (str path "." name)]
           (trace path)
           (concat [(struct block-route (keyword name) [] (list s) path )] (extract-block-routes body path)))
         (extract-block-routes s path))))))

;; wraps extracted block in default bound variables
;; creates shortcuts for request session params and t*
(defn- wrap-extracted-block [eb]
  ;; clojure on defined routes information
  `(fn [request#]
     (let [routes# ~'routes*] ;; create local clojure for routing info
       (binding [~'r* request#
                 ~'s* (:session request#)
                 ~'p* (:params request#)
                 ~'routes* routes# 
                 ~'method* (:method request#)]
         ~@eb))))

;; build routes to all prossible block combinations
(defn- build-routes [name prefix body]
  (letfn [(on-path-lenght [a b] (- (count (:path b)) (count (:path a))))]
    ;; extract codeblock out of body and add complete
    ;; block as well for root functionality
    (let [extracted-blocks (conj (extract-block-routes body "")
                                 (struct block-route :root [] `((html ~@body)) ""))]
      `(apply routes 
              ;; build routemap for easy reference to url
              ;; put in surrounding let to define
              ;; local clojures inside anonumous functions
              (let [~'routes*
                    (hash-map ~@(flatten (map
                                          (fn [eb] [(:name eb) (str prefix (:path eb))])
                                          extracted-blocks)))]
                ;; emit clojure ANY rules after sorting on lenght descending
                ;; since we need longest route to match first
                [~@(for [eb (sort on-path-lenght extracted-blocks)]
                     `(ANY ~(str prefix (:path eb)) [] ~(wrap-extracted-block (:body eb))))])))))

;; API

(defn wrap-block [name params body]
  `(html
    [~(keyword (str "div#" (str name)))
     (let [{:strs ~(vec params)} ~'p*]
       (html ~@body))]))

(defmacro block [name params & body]
  (wrap-block name params body))


(defmacro defblock [name params & body]
  `(def ~name
        (let [~'routes* (if (bound? #'routes*)
                          ~'routes*
                          {})]
          ~(wrap-extracted-block (list (wrap-block name params body))))))

(defmacro defblock [name params & body]
  `(def ~name (struct block-route  nil '~params '~body nil)))

;; defines on root level a set of routes
;; accessing the different blocks inside a page

(defmacro defroutes-page [name prefix & body]
  (binding [in-page* true]
    `(def ~name
          ~(build-routes name prefix body))))

;; utility functions on the bound quick variables
;; renders a specific block
(defmacro callblock [name predefined-block]
  `(block ~name ~(:params (var-get  (resolve predefined-block))) ~@(:body (var-get (resolve predefined-block)))))

;; finds uri for block
(defn block-uri [block-name]
  (routes* block-name))
