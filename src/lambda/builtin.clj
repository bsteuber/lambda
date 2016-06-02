(ns lambda.builtin)

(def builtin
  {'+ [+ [:Number :Number] :Number]})

(def implementation
  (comp first builtin))

(def arg-types
  (comp second builtin))

(def result-type
  (comp #(nth % 2) builtin))
