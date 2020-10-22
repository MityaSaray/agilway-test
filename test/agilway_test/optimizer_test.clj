(ns agilway-test.optimizer-test
  (:require [clojure.test :as t])
  (:require [agilway-test.optimizer :as o]))

(t/deftest simple-optimization
  (t/is (= (o/optimize '(* x 0)) 0)))

(t/run-all-tests #"agilway-test.optimizer-test")