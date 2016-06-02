(ns lambda.core-test
  (:refer-clojure :exclude [eval])
  (:require [lambda.core  :as c]
            [clojure.test :refer [deftest is]]))

(deftest eval
  (is (= [:Number 42]
         (c/eval 42)))
  (is (= [:Number 1]
         (c/eval '((fn [:Number x] x) 1))))
  (is (= [:Number 2]
         (c/eval '(((fn [:Number x]
                      (fn [:Number y]
                        x))
                    2)
                   42))))
  (is (= [:Number 12]
         (c/eval '(+ 8 4))))
  (is (= [:Number 10]
         (c/eval '((fn [:Number x]
                     (+ 8 x))
                   2)))))
