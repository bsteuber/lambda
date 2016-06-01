(ns lambda.term-test
  (:require [lambda.term  :as term]
            [clojure.test :refer [deftest testing is]]))

(def t-var-0 [:var nil 0])
(def t-var-1 [:var nil 1])
(def t-fn-x [:fn nil :x :int
             t-var-0])
(def t-fn-y [:fn nil :y :int
             t-var-1])
(def t-fn-x-y [:fn nil :x :int
               t-fn-y])
(def t-call-x [:call t-fn-x t-var-0])
(def t-call-y [:call t-fn-y t-var-0])
(def s [:var :sub 100])

(deftest shift
  (is (= [:var nil 1]
         (term/shift 1 t-var-0)))
  (is (= [:var nil 2]
         (term/shift 1 t-var-1)))
  (is (= [:fn nil :x :int
          [:var nil 0]]
         (term/shift 1 t-fn-x)))
  (is (= [:fn nil :y :int
          [:var nil 2]]
         (term/shift 1 t-fn-y)))
  (is (= [:fn nil :x :int
          [:fn nil :y :int
           [:var nil 1]]]
         (term/shift 1 t-fn-x-y))))

(deftest substitute-top-var
  (is (= [:var :sub 100]
         (term/substitute-top-var t-var-0 s)))
  (is (= [:var nil 0]
         (term/substitute-top-var t-var-1 s)))
  (is (= [:fn nil :x :int
          [:var nil 0]]
         (term/substitute-top-var t-fn-x s)))
  (is (= [:fn nil :y :int
          [:var :sub 101]]
         (term/substitute-top-var t-fn-y s)))
  (is (= [:fn nil :x :int
          [:fn nil :y :int
           [:var nil 1]]]
         (term/substitute-top-var t-fn-x-y s))))
