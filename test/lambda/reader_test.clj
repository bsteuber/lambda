(ns lambda.reader-test
  (:refer-clojure :exclude [read])
  (:require [lambda.reader :as r]
            [clojure.test  :refer [deftest testing is]]))

(deftest read-number
  (is (= [:int 42]
         (r/read 42)))
  (is (= [:number 0.1]
         (r/read 0.1))))

(deftest read-bool
  (is (= [:bool true]
         (r/read true)))
  (is (= [:bool false]
         (r/read false))))

(deftest read-if
  (is (= [:if [:bool true]
          [:int 1]
          [:int 42]]
         (r/read '(if true 1 42)))))

(deftest read-fn
  (is (= [:fn 'x :Int
          [:var 0]]
         (r/read '(fn [:Int x] x))))
  (is (= [:fn 'x :Int
          [:fn 'y :Int
           [:var 0]]]
         (r/read '(fn [:Int x]
                    (fn [:Int y]
                      y)))))
  (is (= [:fn 'x :Int
          [:fn 'x :Int
           [:var 0]]]
         (r/read '(fn [:Int x]
                    (fn [:Int x]
                      x)))))
  (is (= [:fn 'x :Int
          [:fn 'y :Int
           [:var 1]]]
         (r/read '(fn [:Int x]
                    (fn [:Int y]
                      x)))))
  (is (= [:fn 'x :Int
          [:fn 'y :Int
           [:fn 'z :Int
            [:var 2]]]]
         (r/read '(fn [:Int x]
                    (fn [:Int y]
                      (fn [:Int z]
                        x))))))
  (is (= [:fn 'x :Int
          [:fn 'y :Int
           [:fn 'z :Int
            [:var 1]]]]
         (r/read '(fn [:Int x]
                    (fn [:Int y]
                      (fn [:Int z]
                        y))))))
  (is (= [:fn 'x :Int
          [:fn 'y :Int
           [:fn 'z :Int
            [:var 0]]]]
         (r/read '(fn [:Int x]
                    (fn [:Int y]
                      (fn [:Int z]
                        z)))))))

(deftest read-call
  (is (= [:call
          [:fn 'x :Int [:var 0]]
          [:int 42]]
         (r/read '((fn [:Int x]
                     x)
                   42)))))

(deftest read-builtin
  (is (= [:builtin '+ [[:int 1]
                       [:int 2]]]
         (r/read '(+ 1 2))))
  (is (= [:call
          [:fn 'x :Number
           [:builtin '+ [[:int 8]
                         [:var 0]]]]
          [:int 2]]
         (r/read '((fn [:Number x]
                     (+ 8 x))
                   2)))))
