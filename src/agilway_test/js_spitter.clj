(ns agilway-test.js-spitter
  (:require [clojure.string :as str]))

(defn- gather-variables [args]
  (into #{} (concat (filter symbol? args)
                    (mapcat :args (filter map? args)))))

(defn- expression->js [[func & args]]
  {:args       (gather-variables args)
   :expression (str
                 "("
                 (str/join
                   (str " " func " ")
                   (map
                     (fn [arg]
                       (if (map? arg)
                         (:expression arg)
                         arg))
                     args))
                 ")")})

(defn- to-js [args]
  (if (sequential? args)
    (expression->js (map to-js args))
    args))

(defn ->javascript
  ([name args]
   (let [parsed (to-js args)
         argument-list (str/join ", " (:args parsed))]
     (str
       "function "
       name
       "(" argument-list ")"
       " { return "
       (:expression parsed)
       "; }"))))


(->javascript "hello" '(+ 3 (+ (+ (* z 2) y)) x))
