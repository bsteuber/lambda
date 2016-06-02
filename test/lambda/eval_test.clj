(ns lambda.eval-test
  (:refer-clojure :exclude [eval])
  (:require [lambda.eval  :as e ]
            [clojure.test :refer [deftest testing is]]))

(deftest eval
  (is (= [:number 42]
         (e/eval [:call
                  [:fn 'x :int [:var 0]]
                  [:number 42]]))))
