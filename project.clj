(defproject clocks "0.7"
  :description "A webdsl on top of compojure, see: http://github.com/boymaas/clocks/"
  :dependencies [
                 [org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
                 [compojure "0.4.0-RC3"] 
                 [ring/ring-devel "0.2.0"] 
                 [ring/ring-jetty-adapter "0.2.0"] 
                 [hiccup "0.2.4"] 
                 [scriptjure "0.1.8"]
                 ]
  :dev-dependencies  [[swank-clojure "1.2.0"] 
                      [lein-clojars "0.5.0"]]
  :repositories [["clojars" "http://clojars.org/repo"]]
  )
