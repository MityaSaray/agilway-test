(ns agilway-test.optimizer-test
  (:require [clojure.test :as t])
  (:require [agilway-test.optimizer :as o]))

(o/optimize '(* 1 2 3 x y 4 (+ x y)))

(o/optimize '(* x y))

(o/optimize '(+ 1 2 3 4 (* 1 2 3) x))

(o/optimize '(* 2 3))

;(o/optimize '(- y 3 5 1 (- x 123)))

(t/deftest multiplication-optimization
  (t/is (= (o/optimize '(* (+ x 1 3 2 4) 0)) 0) "Multiple of ANY and zero is zero")
  (t/is (= (o/optimize '(* 1 1 1 1 x)) 'x) "Multiplying by zero element is pointless")
  (t/is (= (o/optimize '(* 3 2 1 x y)) '(* 6 x y)) "We calculate values and keep variables")
  (t/is (= (o/optimize '(* 3 (* 2 1 x (* 5 y)))) '(* 30 x y)) "Should open nested multiplication")
  (t/is (= (o/optimize '(* (* x y) 5)) '(* 5 x y)) "Should put values in first argument place"))

(t/deftest addition-optimization
  (t/is (= (o/optimize '(+ 5 3 2)) 10) "Should sum values")
  (t/is (= (o/optimize '(+ x y 0 5 3 (+ 3 4 z))) '(+ 15 x y z)) "Addition unnesting")
  (t/is (= (o/optimize '(+ x y 1 2 3 (- y 10) (- 10 y))) '(+ 6 x y )) "Subtraction unnesting and evening out")
  (t/is (= (o/optimize '(+ x 1 2 3 (* 2 y) (/ 6 2 ) (/ y 4))) '(+ 9 x (* 2 y) (/ y 4)))
        "Addition should keep order of variables")
  (t/is (= (o/optimize '(+ (+ x y) 1 2 3)) '(+ 6 x y))
        "Will arrange values always before variables"))

(t/deftest division-optimization
  (t/is (= (o/optimize '(/ 0 (+ 3 y) (* 5 z))) 0) "Division is always zero if first element is zero")
  (t/is (= (o/optimize '(/ x y 2 3 5)) '(/ x 30 y)) "Division is first element / multiple of rest")
  (t/is (= (o/optimize '(/ 20 2 z y 2 v)) '(/ 5 z y v)) "Division should try to calculate available number"))

(t/deftest subtraction-optimization
  (t/is (= (o/optimize '(- 5 2 1)) 2) "Should subtract values")
  (t/is (= (o/optimize '(- 5 (+ 2 1) (/ x 6) (* y 2))) '(- 2 (/ x 6) (* 2 y)))
        "Should properly calculate values and preserve order of arguments"))

(t/deftest general-optimization-test
  (let [use-all-expressions '(/ (+ 1 (* 5 3) (- 5 2)) (+ 0 (* 5 (- 3 3 0)) (+ 3 2) (- 5 6) 1))
        use-all-expressions-variables '(/ (+ (y (z * 2)) (- b c 5) 5) (+ y z))]
    (t/is (= (o/optimize use-all-expressions) 19/5))))

(o/optimize '(/ (+ (* y (* z 2)) (- b c 5) 5) (+ y z)))



(t/run-all-tests #"agilway-test.optimizer-test")


(o/optimize '(+ x y 1 2 3 (- y 10) (- 10 y)))