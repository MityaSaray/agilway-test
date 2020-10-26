(ns agilway-test.js-spitter-test
  (:require [clojure.test :as t]
            [agilway-test.js-spitter :as js]))

(t/deftest ->javascript-test
  (t/testing "Javascript spitter"
    (t/is (= "function hello(x) { return (1 + (x * x)); }"
             (js/->javascript "hello" '(+ 1 (* x x)))))
    (t/is (= "function largerFunction(x, z, a, b, y) { return (x + (z + (a + (b + (y + (x + Math.pow(Math.sqrt(Math.abs(1))))))))); }"
             (js/->javascript "largerFunction" '(+ x (+ z (+ a (+ b (+ y (+ x (pow (sqrt (abs 1))))))))))))))
