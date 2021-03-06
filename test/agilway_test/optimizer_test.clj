(ns agilway-test.optimizer-test
  (:require [clojure.test :as t])
  (:require [agilway-test.optimizer :as o]))

(t/deftest multiplication-optimization
  (t/testing
    "Testing multiplication optimizations"
    (t/is (= 4 (o/optimize '(* 2 2))))
    (t/is (= 0 (o/optimize '(* x 0))))
    (t/is (= 'x (o/optimize '(* 1 x))))
    (t/is (= 1 (o/optimize '(* x (/ 1 x)))))
    (t/is (= -1 (o/optimize '(* (- x) (/ 1 x)))))))

(t/deftest addition-optimization
  (t/testing
    "Testing addition optimizations"
    (t/is (= 5) (o/optimize '(+ 3 2)))
    (t/is (= 2) (o/optimize '(+ 5 (- 3))))
    (t/is (= 0 (o/optimize '(+ x (- x)))))
    (t/is (= 'x (o/optimize '(+ 0 x))))
    (t/is (= 'x (o/optimize '(+ x 0))))))

(t/deftest division-optimization
  (t/testing
    "Testing division optimizations"
    (t/is (= 2) (o/optimize '(/ 4 2)))
    (t/is (= 0 (o/optimize '(/ 0 (* x 5)))))
    (t/is (= 1 (o/optimize '(/ x x))))
    (t/is (= -1 (o/optimize '(/ x (- x)))))
    (t/is (= '(* y y) (o/optimize '(/ y (/ 1 y)))))
    (t/is (= '(- (* y y)) (o/optimize '(/ (- y) (/ 1 y)))))))

(t/deftest subtraction-optimization
  (t/testing
    "Testing subtraction optimizations"
    (t/is (= 2 (o/optimize '(- 5 3))))
    (t/is (= 3 (o/optimize '(- 1 (- 2)))))
    (t/is (= 0 (o/optimize '(- x x))))
    (t/is (= '(- x) (o/optimize '(- 0 x))))
    (t/is (= '(- x) (o/optimize '(- (- x) 0))))))

(t/deftest power-optimization
  (t/testing "Pow optimization"
    (t/is (= 0 (o/optimize '(pow 0 5))))
    (t/is (= 4 (o/optimize '(pow 2 2))))
    (t/is (= '(pow 2 y) (o/optimize '(pow 2 y))))))

(t/deftest abs-optimization
  (t/testing "Abs optimization"
    (t/is (= 5 (o/optimize '(abs -5))))
    (t/is (= 'y (o/optimize '(abs (- y)))))
    (t/is (= 'y (o/optimize '(abs y))))))

(t/deftest sqrt-optimization
  (t/testing "Square root optimization"
    (t/is (= 4 (int (o/optimize '(sqrt 16)))))
    (t/is (= '(sqrt y) (o/optimize '(sqrt y))))))

(t/deftest combined-optimizer
  (t/testing "Testing combined optimizers"
    (t/is (= 12 (o/optimize '(+ 2 (abs (- (/ (pow 10 (sqrt 4)) 10)))))))))