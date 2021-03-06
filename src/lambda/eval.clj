(ns lambda.eval
  (:refer-clojure :exclude [eval])
  (:require [lambda.builtin     :as b]
            [lambda.term        :as term]
            [clojure.core.match :refer [match]]
            [lambda.reader      :as r]))

(declare eval-1)

(defn eval-first-non-value [ctx terms]
  (let [[values [next-non-value & more]] (split-with (partial term/value? ctx)
                                                     terms)
        next-evaled (eval-1 ctx next-non-value)]
    (when next-evaled
      (concat values (cons next-evaled more)))))

(defn eval-1 [ctx term]
  (let [val? (partial term/value? ctx)
        vals? (partial every? val?)]
    (match term
      [:call [:fn argname _ body] (arg :guard val?)]
      (term/substitute-top-var body arg)

      [:call (v1 :guard val?) t2]
      (when-let [t2' (eval-1 ctx t2)]
        [:call v1 t2'])

      [:call t1 t2]
      (when-let [t1' (eval-1 ctx t1)]
        [:call t1' t2])

      [:if [:bool true] then _]
      then

      [:if [:bool false] _ else]
      else

      [:if condition then else]
      (when-let [condition' (eval-1 ctx condition)]
        [:if condition' then else])

      [:builtin op (args :guard vals?)]
      (->> args
           (map term/format)
           (apply (b/implementation op))
           r/read)

      [:builtin op args]
      (when-let [args' (eval-first-non-value ctx args)]
        [:builtin op args'])

      [:record (m :guard (comp vals? vals))]
      nil

      [:record m]
      (when-let [vals' (eval-first-non-value ctx (vals m))]
        [:record (zipmap (keys m)
                         vals')])

      [:lookup key (record :guard val?)]
      (match record
        [:record m] (key m))

      [:lookup key record]
      (when-let [record' (eval-1 record)]
        [:lookup key record'])

      :else
      nil)))

(def ^:dynamic *print-steps* false)

(defn eval
  ([term]
   (eval {} term))
  ([ctx term]
   (when *print-steps*
     (prn term))
   (if-let [term' (eval-1 ctx term)]
     (recur ctx term')
     term)))
