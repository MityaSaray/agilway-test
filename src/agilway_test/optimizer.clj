(ns agilway-test.optimizer)

(defn variable? [val]
  (or (symbol? val) (sequential? val)))

(defn split-variables-values [args]
  [(filter variable? args) (filter number? args)])

(defn opposite-value [value]
  (cond
    (and (sequential? value) (= (count value) 2))
    (second value)
    (number? value)
    (- value)
    :else
    `(- ~value)))

(defn unnest-expressions-in-multiplication [args]
  (reduce
    (fn [acc arg]
      (if (and (sequential? arg) (= (first arg) (symbol :*)))
        (concat acc (rest arg))
        (conj acc arg)))
    []
    args))

(defn multiplication-optimizer [args]
  (let [[variables values] (-> args
                               unnest-expressions-in-multiplication
                               split-variables-values)
        values-mult (reduce * values)]
    (cond
      (zero? values-mult)
      0
      (empty? variables)
      values-mult
      (= 1 values-mult)
      (if (> (count variables) 1)
        variables
        (first variables))
      :else
      (conj variables values-mult))))

(defn unnest-expressions-in-addition [args]
  (reduce
    (fn [acc arg]
      (if (sequential? arg)
        (let [func      (first arg)
              arguments (rest arg)]
          (case func
            + (concat acc arguments)
            - (-> acc
                  (conj (first arguments))
                  (concat (map (fn [var]
                                 (opposite-value var))
                               (rest arguments))))
            (conj (vec acc) arg)))
        (conj acc arg)))
    []
    args))


(defn even-out-variables-in-addition [[variables values]]
  (let [grouped    (frequencies variables)
        evened-out (reduce (fn [acc [key value]]
                             (let [negated-key (opposite-value key)]
                               (if-let [negated-val (get acc negated-key)]
                                 (let [delta (- value negated-val)]
                                   (if (pos? delta)
                                     (-> acc (assoc key delta) (dissoc negated-key))
                                     (-> acc (assoc negated-key (- delta)))))
                                 (assoc acc key value))))
                           {}
                           grouped)]
    [(reduce (fn [acc [arg times]] (concat acc (repeat times arg))) [] evened-out) values]))


(defn addition-optimizer [args]
  (let [[variables values] (-> args
                               unnest-expressions-in-addition
                               split-variables-values
                               even-out-variables-in-addition)
        values-sum (reduce + values)]
    (cond
      (empty? variables)
      values-sum
      (zero? values-sum)
      (if (> (count variables) 1)
        variables
        (first variables))
      :else
      (conj variables values-sum))))

(defn division-optimizer [args]
  (let [base                (first args)
        division-multiplier (multiplication-optimizer (rest args))]
    (cond
      (number? base)
      (cond
        (zero? base)
        0
        (number? division-multiplier)
        (/ base division-multiplier)
        (sequential? division-multiplier)
        (if (number? (first division-multiplier))
          (conj (rest division-multiplier) (/ base (first division-multiplier)))
          (conj division-multiplier base))
        :else
        (list base division-multiplier))
      (sequential? base)
      (list base division-multiplier)
      (sequential? division-multiplier)
      (list base division-multiplier)
      :else
      (list base division-multiplier))))

(defn subtraction-optimizer [args]
  (let [base            (first args)
        sum-of-negation (addition-optimizer (rest args))]
    (if (number? base)
      (cond
        (number? sum-of-negation)
        (- base sum-of-negation)
        (and (sequential? sum-of-negation) (number? (first sum-of-negation)))
        (conj (rest sum-of-negation) (- base (first sum-of-negation)))
        :else
        (flatten [base sum-of-negation]))
      (flatten [base sum-of-negation]))))

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
      (if (sequential? optimized-expr)
        (conj optimized-expr func)
        optimized-expr))
    expression))

(agilway-test.evaluator/evaluate {:x 1 :y 1} (optimize '(+ x y 1 2 3 (- y 10) (- 10 y))))

;(optimize '(/ x y 2 3 5))

;(optimize '(/ 20 2 z y 2 v))

;(optimize '(/ (+ (* y (* z 2)) (- b c 5) 5) (+ y z)))

;(optimize '(+ x (- 1 x)))

(optimize '(/ x (* x y)))

(optimize '(/ (* x y) x y))

(optimize '(/ (* x y) 5))

(optimize '(/ x (+ x y)))

(optimize '(/ 5 y))

;(agilway-test.evaluator/evaluate {:y 2 :z 1 :b 1 :c 1} (optimize '(/ (+ (* y (* z 2)) (- b c 5) 5) (+ y z))))
