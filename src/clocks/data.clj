(ns clocks.data)

(defstruct special-form :type :name :params :body :path)

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


