(ns lambda.reader-test
  (:refer-clojure :exclude [read])
  (:require [lambda.reader :as r]
            [clojure.test  :refer [deftest testing is]]))

(deftest read-numbers
  (is (= [:number nil 42]
         (r/read 42))))

(deftest read-fns
  (is (= [:fn nil 'x [:var nil 0]]
         (r/read '(fn [x] x))))
  (is (= [:fn nil 'x
          [:fn nil 'y
           [:var nil 0]]]
         (r/read '(fn [x]
                    (fn [y]
                      y)))))
  (is (= [:fn nil 'x
          [:fn nil 'x
           [:var nil 0]]]
         (r/read '(fn [x]
                    (fn [x]
                      x)))))
  (is (= [:fn nil 'x
          [:fn nil 'y
           [:var nil 1]]]
         (r/read '(fn [x]
                    (fn [y]
                      x)))))
  (is (= [:fn nil 'x
          [:fn nil 'y
           [:fn nil 'z
            [:var nil 2]]]]
         (r/read '(fn [x]
                    (fn [y]
                      (fn [z]
                        x))))))
  (is (= [:fn nil 'x
          [:fn nil 'y
           [:fn nil 'z
            [:var nil 1]]]]
         (r/read '(fn [x]
                    (fn [y]
                      (fn [z]
                        y))))))
  (is (= [:fn nil 'x
          [:fn nil 'y
           [:fn nil 'z
            [:var nil 0]]]]
         (r/read '(fn [x]
                    (fn [y]
                      (fn [z]
                        z)))))))

(deftest read-call
  (is (= [:call nil
          [:fn nil 'x [:var nil 0]]
          [:number nil 42]]
         (r/read '((fn [x]
                     x)
                   42)))))
