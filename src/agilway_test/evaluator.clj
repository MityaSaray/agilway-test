(ns agilway-test.evaluator)

(defn resolve-function [sym]
  (cond
    (= sym (symbol :pow))
    (fn [base & pows]
      (Math/pow base (reduce * pows)))
    (= sym (symbol :abs))
    (fn [val]
      (Math/abs val))
    :else
    (resolve sym)))

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


