(ns lambda.core-test
  (:refer-clojure :exclude [eval])
  (:require [lambda.core  :as c]
            [clojure.test :refer [deftest is]]))

(deftest eval
  (is (= 42
         (c/eval 42)))
  (is (= 1
         (c/eval '((fn [:int x] x) 1))))
  (is (= 2
         (c/eval '(((fn [:int x]
                       (fn [:int y]
                         x))
                    2)
                   42)))))
