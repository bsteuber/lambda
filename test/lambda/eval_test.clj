(ns lambda.eval-test
  (:refer-clojure :exclude [eval])
  (:require [lambda.eval  :as e ]
            [clojure.test :refer [deftest testing is]]))

(deftest eval
  (is (= [:number nil 42]
         (e/eval nil [:call nil
                    [:fn nil 'x [:var nil 0]]
                    [:number nil 42]]))))
