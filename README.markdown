# CLOCKS DEFINES A WEBDSL ON TOP OF COMPOJURE

Implementation of a webdsl on top op compojure, uses ring, compojure and scriptjure as
serverside foundation, and jquery on the clientside.

## Usage

For complete example see `examples/login.clj`.

    (defblock login-form [] 
      [:form {:id :login_form :method :post :url nil}

       (block login-form-fields [email password]
              [:input {:type :text, :id :login-form-email, :value email}]
              [:input {:type :password, :value password}])

       [:button {:id :login-form-submit}]] 

      [:div#login-form-messages]

      (block validate [email]
             [:h2 email]
             (if (not= email "boy")
               [:h3 "incorrect"]
               [:h3 "correct"]))

      (block validate-json [email]
             (str  (if (not= email "boy")
                     {:result true}
                     {:result false})))

      ($defjs

       ;; echo typed messages to div#login-form-messages
       ($id-on-event :login-form-email keyup
                     (var v this.value)
                     ($id-call :login-form-messages html
                               v))

       ;; do some server-isde validation
       ($id-on-event :login-form-email keyup
                     ($id-reload :validate {:email ($id-value :login-form-email)}))

       ;; post a request via jquery to email
       ($id-on-event :login-form-submit click 
                     ($id-reload :login-form-fields {:email (. Math random)})
                     (return false))))

    (defroutes-page index "index"
      [:html
       [:head
        (include-js "/jquery-1.4.2.min.js")]
       (block level1 [] ;; todo check on vector
              [:h1 "Clocks example"]
              [:h2 (block-uri :level1)]

              ;; expands :login-form defined block here ..
              (callblock :login-form login-form)

              ;; print defined routes in this page
              (block level2 []
                     [:ol (for [[k v] routes*]
                            [:li k "-->" [:a.pol {:href v} v]])]))

       (block page-output []
              "page output")]

      ;; do some jquery to load different blocks inside
      ;; the page-output block
      ($defjs
       (. ($ "a.pol") click (fn [event]
                              (var url this.href)
                              (. ($ "#page-output") load url)
                              (. event preventDefault)
                              (return false)))))

## Description

`defroutes-page` can be used to generate a page in which
blocks can be accessed independitly from the rest of the system.

This is implemented by scanning the source for special forms.

`block` indicates a piece of code which can be accessed via a seperate route.
these routes are defined by their path in the tree seperated by dots

    (defroutes-page name prefix
        [:html (block level1 []
               ..code...
               (blcok level2 []
                      ..code..
                      (block level3 []
                             ...code..))))

In this case level3 can be reached by sending a request to:

    prefix.level1.level2.level3

It will only render the ..code... part of block3.

## Javascript and javascript macro's

See for more information `clocks/defjs` and `clocks/jquery`.

   ;; do some server-isde validation
   ($id-on-event :login-form-email keyup
                 ($id-reload :validate {:email ($id-value :login-form-email)}))


## REUSABLE BLOCKS

Reusable blocks can be defined by `defblock`. These can be called with `callblock` from whithin
a page.

    (defblock name [params] ...)

These are called from withing a page using 

    (callblock name var-pointing-to-predefined-block)

# IMPLEMENTATION 

A short explanation of the implementations:

0. expand callblock
   for now all callblocks get expanded first, so the tree walker automaticly finds the correct
   paths and can create a function of them.

1. extract special forms & register routes
   code traverses the tree finding the special forms, currently `block` and `callblock`
   and stores them into a vector capturing all relevant informaiton including
   the path into the code.

2. generate functions and routes
   seperate functions are defined with a prefix so the namespace doesn't get polluted.
   and routes are generated.

## EVALUATION CONTEXT / BINDINGS

Blocks get transformed to anonymous functions with the following thread-local bindings, 
all with a * postfix: 

    (binding [r* request#                     ;; request
            s* (:session request#)          ;; session if available
            p* (:params request#)           ;; params
            routes* routes#                 ;; routes inside page
            method* (:method request#)]     ;; method of request

            ...block code... )

These bindings are present for helper functions. Such as `block-uri` which looks up
the uri of the block by name.

# Licence

Copyright (C) 2010 Boy Maas

Distributed under the Eclipse Public License, the same as Clojure uses. 

# Author

Boy Maas (boy.maas @ gmail.com) http://www.boymaas.nl.


