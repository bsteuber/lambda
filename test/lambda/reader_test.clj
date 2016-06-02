(ns lambda.reader-test
  (:refer-clojure :exclude [read])
  (:require [lambda.reader :as r]
            [clojure.test  :refer [deftest testing is]]))

(deftest read-number
  (is (= [:number 42]
         (r/read 42))))

(deftest read-bool
  (is (= [:bool true]
         (r/read true)))
  (is (= [:bool false]
         (r/read false))))

(deftest read-if
  (is (= [:if [:bool true]
          [:number 1]
          [:number 42]]
         (r/read '(if true 1 42)))))

(deftest read-fn
  (is (= [:fn 'x :int
          [:var 0]]
         (r/read '(fn [:int x] x))))
  (is (= [:fn 'x :int
          [:fn 'y :int
           [:var 0]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      y)))))
  (is (= [:fn 'x :int
          [:fn 'x :int
           [:var 0]]]
         (r/read '(fn [:int x]
                    (fn [:int x]
                      x)))))
  (is (= [:fn 'x :int
          [:fn 'y :int
           [:var 1]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      x)))))
  (is (= [:fn 'x :int
          [:fn 'y :int
           [:fn 'z :int
            [:var 2]]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      (fn [:int z]
                        x))))))
  (is (= [:fn 'x :int
          [:fn 'y :int
           [:fn 'z :int
            [:var 1]]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      (fn [:int z]
                        y))))))
  (is (= [:fn 'x :int
          [:fn 'y :int
           [:fn 'z :int
            [:var 0]]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      (fn [:int z]
                        z)))))))

(deftest read-call
  (is (= [:call
          [:fn 'x :int [:var 0]]
          [:number 42]]
         (r/read '((fn [:int x]
                     x)
                   42)))))

(deftest read-builtin
  (is (= [:builtin '+ [[:number 1]
                       [:number 2]]]
         (r/read '(+ 1 2))))
  (is (= [:call
          [:fn 'x :Number
           [:builtin '+ [[:number 8]
                         [:var 0]]]]
          [:number 2]]
         (r/read '((fn [:Number x]
                     (+ 8 x))
                   2)))))
