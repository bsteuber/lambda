(ns lambda.eval-test
  (:refer-clojure :exclude [eval])
  (:require [lambda.eval  :as e ]
            [clojure.test :refer [deftest testing is]]))

(deftest eval
  (is (= [:int 42]
         (e/eval [:call
                  [:fn 'x :Int [:var 0]]
                  [:int 42]])))
  (is (= [:int 10]
         (e/eval [:builtin '+ [[:number 3]
                               [:number 7]]])))
  (is (= [:number 10.3]
         (e/eval [:builtin '+ [[:number 3.3]
                               [:number 7]]]))))
