(ns lambda.core
  (:refer-clojure :exclude [eval])
  (:require [lambda.eval   :as e]
            [lambda.term   :as t]
            [lambda.type   :as ty]
            [lambda.reader :as r]))

(defn eval [expr]
  (let [term (r/read expr)
        type (ty/type-of term)
        res (e/eval term)
        formatted-res (t/format res)]
    [type formatted-res]))
