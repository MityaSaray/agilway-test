(ns agilway-test.optimizer)

(defn- significant-values [values zero-element]
  (filter (fn [value] (not= value zero-element)) values))

(defn- opposite-value [value]
  (cond
    (and (sequential? value) (= (count value) 2))
    (second value)
    (number? value)
    (- value)
    :else
    (list '- value)))

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
    (and (number? f) (zero? f))
    0
    (every? number? [f s])
    (/ f s)
    :else
    (list '/ f s)))

(defn- subtraction-optimizer [[f s]]
  (if-not s
    (opposite-value f)
    (cond
      (= f s)
      0
      (and (number? f) (zero? f))
      (opposite-value s)
      (and (number? s) (zero? s))
      f
      (every? number? [f s])
      (- f s)
      :else
      (list '- f s))))

(defn optimize [expression]
  (if (sequential? expression)
    (let [func          (first expression)
          args          (map optimize (rest expression))
          optimize-func (case func
                          * multiplication-optimizer
                          / division-optimizer
                          + addition-optimizer
                          - subtraction-optimizer
                          identity)]
      (optimize-func args))
    expression))