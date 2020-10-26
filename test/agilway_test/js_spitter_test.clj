(ns agilway-test.js-spitter-test
  (:require [clojure.test :as t]
            [agilway-test.js-spitter :as js]))

(t/deftest ->javascript-test
  (t/testing "Javascript spitter")
  (t/is (= "function hello(x) { return (1 + (x * x)); }"
           (js/->javascript "hello" '(+ 1 (* x x)))))
  (t/is (= "function largerFunction(x, y, z, a, b) {
  return (x + (z + (a + (b + (y + (x + 1))))))")
        (js/->javascript "largerFunction" '(+ x (+ z (+ a (+ b (+ y (+ x (+ 1))))))))))
