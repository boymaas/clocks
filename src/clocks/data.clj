(ns clocks.data
  (:use clojure.contrib.trace)
  (:use clojure.contrib.str-utils))

;; paths represented as arrays
(defn path->abs-uri
  "see sf->abs-uri .."
  [p]
  (str-join "." p))

(defn path->function-name
  "converts a path [] to a function name always prefixed
with and identifier to prevent namespace collisions"
  [prefix p]
  (symbol (str-join "-" (concat ["clpartial" prefix] p))))

(defn prepend-uri-prefix [uri-prefix routes]
  "prepends uri-prefix to route table" 
  (into {} (for [[k v] routes]
             [k (if (empty? v)
                  uri-prefix
                  (str-join "." [uri-prefix v]))])))

;; Special forms inside code blocks
;; and operations on them
(defstruct special-form :type :name :params :body :path)

(defn sf->function-name
  "see path->function-name"
  [prefix sf]
  (path->function-name prefix (:path sf)))

(defn sf->abs-uri
  "returns a path to the special form"
  [sf & prefix]
  (path->abs-uri (concat prefix (:path sf))))

;; operations on vectors of special forms
(defn vsf->msf
  "converts vector of special forms to map
indexed by name"
  [vsf]
  (into {} (map #(vector (:name %) %) vsf)))

(defn vsf->route-map
  "transforms vsf to a route-map indexed by block name"
  [vsf]
  (into {} (map
            (fn [sf] [(keyword (:name sf)) (sf->abs-uri sf)])
            vsf)))

;; helpers on identifying special forms
(defn symbol-and-eq? [s n]
  (and (seq? s)
       (symbol? (first s))
       ;; symbols resolve to same namespace var
       (= (symbol (name  (first s))) n)))

(defn is-block? [s]
  (or  (symbol-and-eq? s 'block)
       (symbol-and-eq? s 'page)))

(defn is-callblock? [s]
  (symbol-and-eq? s 'callblock))

(defn block-name [b]
  (second b))


(defn keyword->symbol [k]
  (symbol  (subs (str k) 1)))
