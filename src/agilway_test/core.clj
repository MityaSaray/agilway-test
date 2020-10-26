(ns agilway-test.core
  (:require [agilway-test.evaluator :as e]
            [agilway-test.optimizer :as o]
            [agilway-test.js-spitter :as js]))

(def evaluate e/evaluate)

(def optimize o/optimize)

(def ->javascript js/->javascript)

(defn main
  [& args]
  (println "Use through repl i guess"))