(ns lambda.core
  (:refer-clojure :exclude [eval])
  (:require [lambda.eval   :as e]
            [lambda.term   :as t]
            [lambda.reader :as r]))

(def eval
  (comp t/format
        e/eval
        r/read))
