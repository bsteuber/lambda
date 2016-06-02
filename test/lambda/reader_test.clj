(ns lambda.reader-test
  (:refer-clojure :exclude [read])
  (:require [lambda.reader :as r]
            [clojure.test  :refer [deftest testing is]]))

(deftest read-number
  (is (= [:number nil 42]
         (r/read 42))))

(deftest read-bool
  (is (= [:bool nil true]
         (r/read true)))
  (is (= [:bool nil false]
         (r/read false))))

(deftest read-if
  (is (= [:if nil [:bool nil true]
          [:number nil 1]
          [:number nil 42]]
         (r/read '(if true 1 42)))))

(deftest read-fn
  (is (= [:fn nil 'x :int
          [:var nil 0]]
         (r/read '(fn [:int x] x))))
  (is (= [:fn nil 'x :int
          [:fn nil 'y :int
           [:var nil 0]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      y)))))
  (is (= [:fn nil 'x :int
          [:fn nil 'x :int
           [:var nil 0]]]
         (r/read '(fn [:int x]
                    (fn [:int x]
                      x)))))
  (is (= [:fn nil 'x :int
          [:fn nil 'y :int
           [:var nil 1]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      x)))))
  (is (= [:fn nil 'x :int
          [:fn nil 'y :int
           [:fn nil 'z :int
            [:var nil 2]]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      (fn [:int z]
                        x))))))
  (is (= [:fn nil 'x :int
          [:fn nil 'y :int
           [:fn nil 'z :int
            [:var nil 1]]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      (fn [:int z]
                        y))))))
  (is (= [:fn nil 'x :int
          [:fn nil 'y :int
           [:fn nil 'z :int
            [:var nil 0]]]]
         (r/read '(fn [:int x]
                    (fn [:int y]
                      (fn [:int z]
                        z)))))))

(deftest read-call
  (is (= [:call nil
          [:fn nil 'x :int [:var nil 0]]
          [:number nil 42]]
         (r/read '((fn [:int x]
                     x)
                   42)))))
