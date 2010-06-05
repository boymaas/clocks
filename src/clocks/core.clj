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
(declare r* s* p* t* method*)

(defmacro defblock [name params & body]
  `(html
    [~(keyword (str "div#" (str name)))
     (let [{:strs ~(vec params)} ~'p*]
       ~@body)]))

;; extracting defblock from declaration
;; and associating these blocks with a specific
;; route
(defstruct block-route :path :body)
(defn- extract-block-routes [body path]
  "parses defblocks recursively
   returning tree with route"
  (flatten
   (for [s (filter sequential? body)]
     (if (= (first s) 'defblock)
       (let [[_ name & body] s
             path (str path "." name)]
         (concat [(struct block-route path (list s))] (extract-block-routes body path)))
       (extract-block-routes s path)))))

;; wraps extracted block in default bound variables
;; creates shortcuts for request session params and t*
(defn- wrap-extracted-block [eb]
  `(fn [request#]
     (binding [~'r* request#
               ~'s* (:session request#)
               ~'p* (:params request#)
               ~'t* {}
               ~'method* (:method request#)]
       ~@eb)))

;; build routes to all prossible block combinations
(defn build-routes [name prefix body]
  (letfn [(on-path-lenght [a b] (- (count (:path b)) (count (:path a))))]
  ;; extract codeblock out of body and add complete
  ;; block as well
    (let [extracted-blocks (conj (extract-block-routes body "")
                                 (struct block-route "" `((html ~@body))))]
      (for [eb (sort on-path-lenght extracted-blocks)]
        `(ANY ~(str prefix (:path eb)) [] ~(wrap-extracted-block (:body eb)))))))

;; defines on root level a set of routes
;; accessing the different blocks inside a page
(defmacro defroutes-page [name prefix & body]
  `(defroutes ~name
        ~@(build-routes name prefix body)))

;; utility functions on the bound quick variables

;; renders a specific block
(defn callblock [name])

;; finds uri for block
(defn block-url [block-name]
  "block-url")
