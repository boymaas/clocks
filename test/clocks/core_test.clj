(ns clocks.core-test
  (:use [clocks.core] :reload-all)
  (:use [clojure.test]))

;; simple definition defining
;; all functionalities
(defblock defined-block [db-p1 db-p2]
  [:p "defined-block db-p1: [" db-p1 "] db-p2: [" db-p2 "]"])

(defpage page [page-p1 page-p2]
  [:page
    (block level1 [level1-p1 level1-p2]
           [:h1  "level1"]
           (callblock :defined-block level2))])

(def page-routes (PAGE "/" page))

(deftest partial-output ;; FIXME: write
  (is (md5= "c1bb" (cp-params "/" page))))

