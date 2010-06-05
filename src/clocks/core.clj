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
                         seq-utils
                         str-utils)
        hiccup.core
        compojure.core))

;; the to be bound variables
(declare r* s* p* method* routes*)

(defmacro defblock [name params & body]
  `(html
    [~(keyword (str "div#" (str name)))
     (let [{:strs ~(vec params)} ~'p*]
       (html ~@body))]))

;; extracting defblock from declaration
;; and associating these blocks with a specific
;; route
(defstruct block-route :name :path :body)
(defn- extract-block-routes [body path]
  "parses defblocks recursively
   returning tree with route"
  (flatten
   (for [s (filter sequential? body)]
     (if (= (first s) 'defblock)
       (let [[_ name & body] s
             path (str path "." name)]
         (concat [(struct block-route (keyword name) path (list s))] (extract-block-routes body path)))
       (extract-block-routes s path)))))

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
                                 (struct block-route :root "" `((html ~@body))))]
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

;; defines on root level a set of routes
;; accessing the different blocks inside a page
(defmacro defroutes-page [name prefix & body]
  `(def ~name
        ~(build-routes name prefix body)))

;; utility functions on the bound quick variables

;; renders a specific block
(defn callblock [name])

;; finds uri for block
(defn block-uri [block-name]
  (routes* block-name))
