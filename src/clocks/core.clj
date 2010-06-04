(ns clocks.core
  (:use (clojure.contrib ns-utils
                         seq-utils)
        compojure.core))

(declare r* s* p* t* method*)

(defstruct block :name :fn)

;; renders a specific block
(defn callblock [name])

;; render tree structure of all defined cblocks inside code
;; bind variables
;;   insert parent
;;   append children
(defn- f->t [f t body? s]
  "changes function call from f to t
   and removes the body from the definition if required"
  (if (= f (first s))
    `(~t ~@(if body?
              (rest s)
              (list (second s))))
    s))

(def defblock->callblock (partial f->t 'defblock 'callblock false))

(defmacro defblock- [name params & body]
  `[(create-struct block ~name
                   (fn [~'request]
                    (let [{:strs ~(vec params)} (~'request :params)]
                       ~@(map defblock->callblock body))))

    ~@(filter #(= 'defblock (first %)) body)])

(defmacro defblock [name params & body]
  `(do (let [{:strs ~(vec params)} ~'p*]
         ~@(map defblock->callblock body))

       ~@(filter #(= 'defblock (first %)) body)))

(defmacro defpage [name base-uri & body]
  `(def ~name (ANY ~(str base-uri "*") request# (dispatch ~base-uri request# (defblock ~name [] ~@body)))))


(defn dispatch [base-uri request tree]
  (fn [request]
    (binding [r* request
              s* (:session request)
              p* (:params request)
              t* tree
              method* (:method request)]

      ;; subtract base uri from request
      ;; on nothing render root
      ;; on path
      ;; find block & render block
      
      )))


(defn block-url [block-name]
  "block-url")
