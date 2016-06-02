(ns lambda.reader
  (:refer-clojure :exclude [read])
  (:require [clojure.core.match :refer [match]]
            [lambda.builtin     :as b]
            [lambda.util        :refer [map-vals]]))

(defn boolean? [b]
  (contains? #{true false} b))

(defn read
  ([expr]
   (read {} 0 expr))
  ([ctx depth expr]
   (match
     expr

     (['if condition then else] :seq)
     [:if
      (read ctx depth condition)
      (read ctx depth then)
      (read ctx depth else)]


     ([(op :guard b/builtin) & args] :seq)
     [:builtin op (for [arg args]
                    (read ctx depth arg))]

     (['fn [arg-type
            (arg :guard symbol?)]
       body] :seq)
     (let [read-body (read (assoc ctx arg (inc depth))
                           (inc depth)
                           body)]
       [:fn arg arg-type read-body])

     ([f arg] :seq)
     [:call
      (read ctx depth f)
      (read ctx depth arg)]

     (sym :guard symbol?)
     [:var (or (- depth (ctx sym))
               (throw (ex-info "Var not found"
                               {:context ctx
                                :var sym})))]

     (x :guard number?)
     [:number x]

     (b :guard boolean?)
     [:bool b]

     (m :guard map?)
     [:record (map-vals (partial read ctx depth) m)])))
