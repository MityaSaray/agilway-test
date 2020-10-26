# agilway-test

Program is tested on java 11, clojure 1.10.

It uses leiningen and is intended to be used from repl / run test only.

To run test type `lein test`

To run repl type `lein repl`

In main namespace exposed all 3 namespaces with aliases e/evaluator, o/optimizer, js/to-javascript

Also functions evaluate, optimize, ->javascript are available in namespace agilway-test.core