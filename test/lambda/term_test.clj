(ns lambda.term-test
  (:refer-clojure :exclude [format])
  (:require [lambda.term  :as term]
            [clojure.test :refer [deftest testing is]]))

(def t-var-0 [:var 0])
(def t-var-1 [:var 1])
(def t-fn-x [:fn 'x :int
             t-var-0])
(def t-fn-y [:fn 'y :int
             t-var-1])
(def t-fn-x-y [:fn 'x :int
               t-fn-y])
(def t-call-x [:call t-fn-x t-var-0])
(def t-call-y [:call t-fn-y t-var-0])
(def s [:var 100])

(deftest format-nested-fn
  (is (= '(fn [:int x] (fn [:int y] x))
         (term/format t-fn-x-y))))

(deftest format-record
  (is (= '{:x 1
           :y true}
         (term/format [:record {:x [:number 1]
                                :y [:bool true]}]))))

(deftest shift
  (is (= [:var 1]
         (term/shift 1 t-var-0)))
  (is (= [:var 2]
         (term/shift 1 t-var-1)))
  (is (= [:fn 'x :int
          [:var 0]]
         (term/shift 1 t-fn-x)))
  (is (= [:fn 'y :int
          [:var 2]]
         (term/shift 1 t-fn-y)))
  (is (= [:fn 'x :int
          [:fn 'y :int
           [:var 1]]]
         (term/shift 1 t-fn-x-y))))

(deftest substitute-top-var
  (is (= [:var 100]
         (term/substitute-top-var t-var-0 s)))
  (is (= [:var 0]
         (term/substitute-top-var t-var-1 s)))
  (is (= [:fn 'x :int
          [:var 0]]
         (term/substitute-top-var t-fn-x s)))
  (is (= [:fn 'y :int
          [:var 101]]
         (term/substitute-top-var t-fn-y s)))
  (is (= [:fn 'x :int
          [:fn 'y :int
           [:var 1]]]
         (term/substitute-top-var t-fn-x-y s))))
