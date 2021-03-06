# Clocks defines a webdsl on top of compojure

Implementation of a webdsl on top op compojure/ring and scriptjure as
serverside foundation, jquery can be used on the client side.

Goal is to abstracts away most of the dependencies and tedious boilerplate we all hate so much. 

## Usage
    (use 'clocks.core)

    (defblock my-reusable-interactive-block [param1 param2 param3 ...]
        ..code.. )

    (defpage my-interactive-page [param1 param2]
        [:html (block level1 []
               ..code...
               (blcok level2 []
                      ..code..
                      (block level3 []
                             ...code..
                             (callblock :my-reusable-interactive-block :local-name-of-block))))

    (defroutes example
       (PAGE "/ajax-page" my-interactive-page))

This definition will generate routes to all defined blocks.

    /ajax-page.level1.level2.level3 --will-render--> level3

and the complete page render at:
 
    /ajax-page --will-render--> ajax-page

## API

`defpage` can be used to generate a page in which
`blocks` can be accessed independitly from the rest of the system.

`block` indicates a piece of code which can be accessed via a seperate route.
these routes are defined by their path in the tree seperated by dots

`PAGE` a new route parameter for compojure.

       (PAGE "/" index)

This will generate all the routes neccessary to be able to render all individual defined blocks
individually.


## Helpers

### Statefull session

`clocks-session-get` and `clocks-session-put!` can be used to update session information. 

### Routing

`clocks-uri :name` will find the uri to render a certain block.
`clocks-uri-this` will return the uri of the "lexical" block.

The uri of the block which is executed is in the request uri ... 

## Javascript and javascript macro's

See for more information `clocks/defjs` and `clocks/jquery`.

    ($id-on-event :login-form-email keyup
                 ($id-reload :validate {:email ($id-value :login-form-email)}))

## REUSABLE BLOCKS

Reusable blocks can be defined by `defblock`. These can be called with `callblock` from whithin
a page.

    (defblock name [params] ...)

These are called from withing a page using 

    (callblock name var-pointing-to-predefined-block)

## IMPLEMENTATION 

A short explanation of the implementations:

0. *expand callblock*
   for now all callblocks get expanded first, so the tree walker automaticly finds the correct
   paths and can create a function of them.

1. *extract special forms & register routes*
   code traverses the tree finding the special forms, currently `block` and `callblock`
   and stores them into a vector capturing all relevant informaiton including
   the path into the code.

2. *generate functions and routes*
   seperate functions are defined with a standard prefix so the namespace doesn't get polluted.

3. *generate routes*
   `(PAGE "route" page-id)` macro generates all the routes needed for the seperate blocks.

4. *per block session and parameter wrapping* special vars are introduced for the blocks and
   helper functions. Binding is done over the called block. Extra bindings can be provided
   wrapping the routes as usual using a ring handler. 
   `r* s* p* method* routes*`

# Larger example

For complete example see `examples/login.clj`.

# Licence

Copyright (C) 2010 Boy Maas

Distributed under the Eclipse Public License, the same as Clojure uses. 

# Author

Boy Maas (boy.maas @ gmail.com) http://www.boymaas.nl.


