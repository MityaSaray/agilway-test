(ns agilway-test.optimizer
  (:require [agilway-test.evaluator :as ev]))

(defn- significant-values [values zero-element]
  (filter (fn [value] (not= value zero-element)) values))

(defn- zero-num? [num]
  (and (number? num) (zero? num)))

(defn- negative-value? [value]
  (cond
    (number? value)
    (neg? value)
    (sequential? value)
    (and (= (count value) 2) (= :- (keyword (first value))))
    :else
    false))

(defn- solve-sign [args val]
  (let [negative-count (filter negative-value? args)]
    (if (= (count negative-count) 1)
      (if (number? val)
        (- val)
        (list '- val))
      val)))

(defn- opposite-value [value]
  (cond
    (negative-value? value)
    (second value)
    (number? value)
    (- value)
    :else
    (list '- value)))

(defn to-positive [value]
  (if (negative-value? value)
    (opposite-value value)
    value))

(defn- inverted-values?
  ([[f s]]
   (or (inverted-values? f s)
       (inverted-values? s f)))
  ([f s]
   (let [[f s] (map to-positive [f s])]
     (= (list '/ 1 f) s))))

(defn- multiplication-optimizer [args]
  (if (some #{0} args)
    0
    (let [significant-multipliers (significant-values args 1)]
      (cond
        (empty? significant-multipliers)
        1
        (= 1 (count significant-multipliers))
        (first significant-multipliers)
        (every? number? args)
        (reduce * args)
        (inverted-values? args)
        (solve-sign args 1)
        :else
        (conj significant-multipliers '*)))))

(defn- addition-optimizer [[f s]]
  (if (= (opposite-value f) s)
    0
    (let [significant-elements (significant-values [f s] 0)]
      (cond
        (empty? significant-elements)
        0
        (every? number? [f s])
        (+ f s)
        (= 1 (count significant-elements))
        (first significant-elements)
        :else
        (conj significant-elements '+)))))

(defn- division-optimizer [[f s]]
  (cond
    (= f s)
    1
    (= f (opposite-value s))
    -1
    (zero-num? f)
    0
    (and (number? s) (= 1 s))
    f
    (every? number? [f s])
    (/ f s)
    (inverted-values? f s)
    (solve-sign [f s] (multiplication-optimizer (map to-positive [f f])))
    :else
    (list '/ f s)))

(defn- subtraction-optimizer [[f s]]
  (if-not s
    (opposite-value f)
    (cond
      (= f s)
      0
      (zero-num? f)
      (opposite-value s)
      (zero-num? s)
      f
      (every? number? [f s])
      (- f s)
      :else
      (list '- f s))))

(defn- power-optimizer [[f s]]
  (cond
    (zero-num? f)
    0
    (every? number? [f s])
    (ev/pow f s)
    :else
    (list 'pow f s)))

(defn- sqrt-optimizer [[value]]
  (cond
    (zero-num? value)
    0
    (number? value)
    (ev/sqrt value)
    :else
    (list 'sqrt value)))

(defn- abs-optimizer [[value]]
  (cond
    (number? value)
    (ev/abs value)
    (negative-value? value)
    (opposite-value value)
    :else
    value))

(def OPTIMIZERS {:*    multiplication-optimizer
                 :/    division-optimizer
                 :+    addition-optimizer
                 :-    subtraction-optimizer
                 :pow  power-optimizer
                 :sqrt sqrt-optimizer
                 :abs  abs-optimizer})

(defn optimize [expression]
  (if (sequential? expression)
    (let [func          (first expression)
          args          (map optimize (rest expression))
          optimize-func (get OPTIMIZERS (keyword func) identity)]
      (optimize-func args))
    expression))
