(ns agilway-test.optimizer)

(defn variable? [val]
  (or (symbol? val) (sequential? val)))

(defn split-variables-values [args]
  [(filter variable? args) (filter number? args)])

(defn multiplication-optimizer [args]
  (let [[variables values] (split-variables-values args)
        values-mult (reduce * values)]
    (cond
      (zero? values-mult)
      0
      (empty? variables)
      values-mult
      (= 1 values-mult)
      variables
      :else
      (conj variables values-mult))))

(defn division-optimizer [args]
  (let [base (first args)]
    (if (and (number? base) (zero? base))
      0
      (conj (multiplication-optimizer (rest args)) base))))

(defn addition-optimizer [args]
  (let [[variables values] (split-variables-values args)
        values-sum (reduce + values)]
    (cond
      (empty? variables)
      values-sum
      (zero? values-sum)
      (if (> (count variables) 1)
        variables
        (first variables))
      :else
      (conj variables values-sum)))
  )

(defn subtraction-optimizer [args]
  (conj (addition-optimizer (rest args)) (first args)))

(defn optimize [expression]
  (if (sequential? expression)
    (let [func           (first expression)
          args           (map optimize (rest expression))
          optimize-func  (case func
                           * multiplication-optimizer
                           / division-optimizer
                           + addition-optimizer
                           - subtraction-optimizer
                           identity)
          optimized-expr (optimize-func args)]
      (println (optimize-func args))
      (if (sequential? optimized-expr)
        (conj optimized-expr func)
        optimized-expr))
    expression))

(optimize '(* 1 2 3 x y 4 (+ x y)))

(optimize '(* x y))

(optimize '(+ 1 2 3 4 (* 1 2 3) x))

(optimize '(* 2 3))

(optimize '(/ y 2 2 x))

(optimize '(- y 3 5 1 x 123))