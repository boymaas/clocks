(ns clocks.core-test
  (:use [clocks.core] :reload-all)
  (:use [clojure.test]))

;; simple definition defining
;; all functionalities
(defblock defined-block [db-p1 db-p2]
  [:p "defined-block db-p1: [" db-p1 "] db-p2: [" db-p2 "]"])

(defpage page [page-p1 page-p2]
  [:page
   [:p page-p1 " " page-p2]
    (block level1 [level1-p1 level1-p2]
           [:h1  "level1"]
           [:p level1-p1 " " level1-p2]
           (callblock :defined-block level2))])

(def page-routes (PAGE "/" page))

;; partial output
(deftest partial-output ;; FIXME: write
  (is (md5= "c69c" (cp-params "/" page)) "renders ok without params")
  (is (md5= "7041" (cp-params "/" page :db-p1 "db-p1" :db-p2 "db-p2")) "called block takes params")
  (is (md5= "8dbc" (cp-params "/" page :level1-p1 "level1-p1" :level1-p2 "level1-p2")) "nested block takes param")
  ;; try some nested patrials, take deepest
  ;; it that works high probability lesser will also work
  (is (md5= "1dba" (cp-params "/" page-level1-level2)))
  (is (md5= "5801" (cp-params "/" page-level1-level2 :db-p1 "db-p1" :db-p2 "db-p2")) "called block takes params")
  )


