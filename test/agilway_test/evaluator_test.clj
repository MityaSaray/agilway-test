(ns agilway-test.evaluator-test
  (:require [clojure.test :as t])
  (:require [agilway-test.evaluator :as e]))

(t/deftest addition-evaluation
  (t/is (= (e/evaluate {} '(+ 1 (+ 1 2 3))) 7) "Addition should work"))

(t/deftest subtraction-evaluation
  (t/is (= (e/evaluate {} '(- 5 (- 2 1 3))) 7) "Subtraction should work"))

(t/deftest multiplication-evaluation
  (t/is (= (e/evaluate {} '(* 2 (* 2 2 2))) 16) "Multiplication should work"))

(t/deftest division-evaluation
  (t/is (= (e/evaluate {} '(/ 8 (/ 4 2 2))) 8) "Division should work"))

(t/deftest power-evaluation
  (t/is (= (e/evaluate {} '(pow 2 (pow 2 2 2))) (int (Math/pow 2 16))) "Power should work"))

(t/deftest abs-evaluation
  (t/is (= (e/evaluate {} '(abs -10)) 10) "Abs should work"))

(t/deftest sqrt-evaluation
  (t/is (= (int (e/evaluate {} '(sqrt 169))) 13))
  (t/is (< (Math/abs ^double (- (e/evaluate {} '(sqrt 10e5)) (Math/sqrt (float 10e5)))) 1e-5)))

(t/deftest simple-evaluation
  (let [expression '(* 2 (+ 1 1))]
    (t/is (e/evaluate {} expression) 4)))

(t/deftest complex-evaluation
  (let [expression '(abs (* -1 2 (+ 1 2) (- (/ (pow 10 3) 100) 5)))]
    (t/is (= (e/evaluate {} expression) 30))))

(t/deftest simple-argument-evaluation
  (t/is (= (e/evaluate {:x 10} 'x)) 10))

(t/deftest complex-arguments-evaluation
  (t/is (= (e/evaluate {:x 2 :y 3 :z 4} '(+ x x (* y z))) 16)))

