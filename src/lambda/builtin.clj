(ns lambda.builtin)

(def builtin
  {'+ [+ [:Number :Number] :Number]
   '- [- [:Number :Number] :Number]
   'inc [inc [:Number] :Number]
   'dec [dec [:Number] :Number]
   'zero? [zero? [:Number] :Bool]})

(def implementation
  (comp first builtin))

(def arg-types
  (comp second builtin))

(def result-type
  (comp #(nth % 2) builtin))
