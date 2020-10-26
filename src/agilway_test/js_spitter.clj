(ns agilway-test.js-spitter
  (:require [clojure.string :as str]))

(defn- gather-variables [args]
  (distinct (concat (filter symbol? args)
                    (mapcat :args (filter map? args)))))

(defn- extract-expression-or-val [args]
  (map
    (fn [arg]
      (if (map? arg)
        (:expression arg)
        arg))
    args))

(defn expression-to-string [func args]
  (case (keyword func)
    :sqrt (str "Math.sqrt(" (first (extract-expression-or-val args)) ")")
    :pow (str "Math.pow(" (str/join ", " (extract-expression-or-val args)) ")")
    :abs (str "Math.abs(" (first (extract-expression-or-val args)) ")")
    (str
      "("
      (str/join
        (str " " func " ")
        (extract-expression-or-val args)
        )
      ")")))

(defn- expression->js [[func & args]]
  {:args       (gather-variables args)
   :expression (expression-to-string func args)})

(defn- to-js [args]
  (if (sequential? args)
    (expression->js (map to-js args))
    args))

(defn ->javascript
  ([name args]
   (let [parsed        (to-js args)
         argument-list (str/join ", " (:args parsed))]
     (str
       "function "
       name
       "(" argument-list ")"
       " { return "
       (:expression parsed)
       "; }"))))

(->javascript "hello" '(+ x (+ y (+ x z))))
