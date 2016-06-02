(ns lambda.type-test
  (:refer-clojure :exclude [>])
  (:require [lambda.core   :as c]
            [lambda.reader :as r]
            [lambda.type   :as ty]
            [clojure.test  :refer [deftest is]]))

(deftest >
  (is (ty/> :Number :Number))
  (is (ty/> :Top :Number))
  (is (ty/> [:Fn :Number :Top]
            [:Fn :Number :Bool]))
  (is (ty/> [:Fn :Number :Bool]
            [:Fn :Top :Bool]))
  (is (ty/> [:Fn :Number :Top]
            [:Fn :Top :Bool]))
  (is (ty/> [:Record {:x :Number}]
            [:Record {:x :Number
                      :y :Bool}]))
  (is (ty/> [:Record {:x :Top}]
            [:Record {:x :Number}])))

(deftest type-of
  (is (= :Int
         (ty/type-of (r/read 42))))
  (is (= :Number
         (ty/type-of (r/read 42.34))))
  (is (= :Bool
         (ty/type-of (r/read true))))

  (is (= :Int
         (ty/type-of (r/read '(if ((fn [:Number x]
                                     true)
                                   42)
                                1
                                10)))))
  (is (= [:Fn :Number :Number]
         (ty/type-of (r/read '(fn [:Number x]
                                x)))))
  (is (= [:Fn :Number :Int]
         (ty/type-of (r/read '(fn [:Number x]
                                42)))))
  (is (= :Number
         (ty/type-of (r/read '((fn [:Number x]
                                 x)
                               18)))))
  (is (= :Int
         (ty/type-of (r/read '((fn [:Top x]
                                 42)
                               false))))))
