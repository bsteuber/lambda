(ns lambda.eval-test
  (:refer-clojure :exclude [eval])
  (:require [lambda.eval  :as e ]
            [clojure.test :refer [deftest testing is]]))

(deftest eval
  (is (= [:number 42]
         (e/eval [:call
                  [:fn 'x :int [:var 0]]
                  [:number 42]])))
  (is (= [:number 10]
         (e/eval [:builtin '+ [[:number 3]
                               [:number 7]]]))))

(deftest eval-generic
  (is (= [:number 42]
         (e/eval [:apply-generic
                  [:generic 'X [:number 42]]
                  :bool])))
  (is (= [:fn 'x :bool [:var 0]]
         (e/eval [:apply-generic
                  [:generic 'X [:fn 'x [:Type-Var 0] [:var 0]]]
                  :bool])))
  (is (= [:bool true]
         (e/eval [:call
                  [:apply-generic
                   [:generic 'X [:fn 'x [:Type-Var 0] [:var 0]]]
                   :bool]
                  [:bool true]]))))
