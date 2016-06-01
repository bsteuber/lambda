(ns lambda.eval
  (:refer-clojure :exclude [eval])
  (:require [lambda.term        :as term]
            [clojure.core.match :refer [match]]))

(defn eval-1 [ctx term]
  (let [val? (partial term/value? ctx)]
    (match
     term
     [:call _ [:fn _ argname body] (arg :guard val?)]
     (term/substitute-top-var body arg)

     [:call info (v1 :guard val?) t2]
     (when-let [t2' (eval-1 ctx t2)]
       [:call info v1 t2'])

     [:call info t1 t2]
     (when-let [t1' (eval-1 ctx t1)]
       [:call info t1' t2])

     :else
     nil)))

(defn eval
  ([term]
   (eval {} term))
  ([ctx term]
   (if-let [term' (eval-1 ctx term)]
     (recur ctx term')
     term)))
