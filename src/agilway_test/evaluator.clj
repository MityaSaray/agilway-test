(ns agilway-test.evaluator)

(def SQRT-DELTA 1e-5)

(defn abs [val]
  (if (pos? val)
    val
    (- val)))

(defn sqrt
  ([number]
   (sqrt number number))
  ([number approx]
   (let [guess (/ (+ number (/ approx number)) 2)]
     (if (> (abs (- number guess)) SQRT-DELTA)
       (recur guess approx)
       guess))))

(defn pow [base & pows]
  (apply * (repeat (reduce * pows) base)))

(def func-map
  {(symbol :pow)  pow
   (symbol :abs)  abs
   (symbol :sqrt) sqrt})

(defn resolve-function [sym]
  (get func-map sym (resolve sym)))

(defn evaluate [fn-args arg]
  (cond
    (sequential? arg)
    (apply
      (resolve-function (first arg))
      (map
        (fn [a]
          (evaluate fn-args a))
        (rest arg)))
    (symbol? arg)
    ((keyword arg) fn-args)
    :else
    arg))