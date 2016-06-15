(ns lambda.type-test
  (:require [lambda.reader :as r]
            [lambda.type   :as ty]
            [clojure.test  :refer [deftest is]]))

(deftest substitute
  (is (= [:Fn :Bool :Bool]
         (ty/substitute-top-var [:Fn
                                 [:Type-Var 0]
                                 [:Type-Var 0]]
                                :Bool)))
  (is (= [:Record
          {:x :Number
           :y [:Type-Var 0]}]
         (ty/substitute-top-var [:Record
                                 {:x [:Type-Var 0]
                                  :y [:Type-Var 1]}]
                                :Number))))

(deftest type-of
  (is (= :Number
         (ty/type-of (r/read 42))))
  (is (= :Bool
         (ty/type-of (r/read true))))

  (is (= :Number
         (ty/type-of (r/read '(if ((fn [:Number x]
                                     true)
                                   42)
                                1
                                10)))))
  (is (= [:Fn :Number :Number]
         (ty/type-of (r/read '(fn [:Number x]
                                x)))))
  (is (= [:Fn :Number :Number]
         (ty/type-of (r/read '(fn [:Number x]
                                42)))))
  (is (= :Number
         (ty/type-of (r/read '((fn [:Number x]
                                 x)
                               18))))))
