(ns lambda.core-test
  (:refer-clojure :exclude [eval])
  (:require [lambda.core  :as c]
            [lambda.eval  :as e]
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
                   2))))
  (is (= [:Bool true]
         (c/eval '(zero? (- 5 5)))))
  (is (= [:Number 10]
         (c/eval '(+ (+ 1 0)
                     (+ 2 7)))))
  (is (= [:Number 8]
         (c/eval '(((fn [:Number x]
                       (fn [:Number y]
                         (+ x y)))
                    2)
                   6))))
  (is (= [[:Record {:x :Number}] {:x 1}]
         (c/eval '{:x 1})))
  (is (= [[:Record {:x :Number
                    :y :Bool}] {:x 1
                                :y false}]
         (c/eval '{:x (if true 1 2)
                   :y (zero? 1)})))
  (is (= [:Number 42]
         (c/eval '(:x {:x 42
                       :y false}))))
  (is (= [:Number 42]
         (c/eval '((fn [[:Record {:x :Number}] rec]
                     (:x rec))
                   {:x (if true 42 0)})))))

(deftest eval-generic
  (is (= '[[:Fn :Number :Bool]
           (fn [:Number x]
             true)]
         (c/eval '(apply-generic
                   (generic X (fn [[:Type-Var 0] x]
                                true))
                   :Number))))
  (is (= '[:Bool
           true]
         (c/eval '((apply-generic
                    (generic X (fn [[:Type-Var 0] x]
                                 true))
                    :Number)
                   42)))))

(deftest eval-existential
  (is (= [:Number 1]
         (binding [e/*print-steps* true]
           (c/with-error-pprint
             (c/eval '(let [[T f] (pack :Number
                                        (fn [:Number x]
                                          x)
                                        [:Some T [:Fn :Number [:Type-Var 0]]])]
                        (f 1)))))))
  )
