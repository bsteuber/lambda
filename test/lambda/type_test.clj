(ns lambda.type-test
  (:require [lambda.reader :as r]
            [lambda.type   :as ty]
            [clojure.test  :refer [deftest is]]))

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
