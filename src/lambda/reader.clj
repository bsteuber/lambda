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
   (let [rd (partial read ctx depth)]
     (match expr
       (['if condition then else] :seq)
       [:if (rd condition) (rd then) (rd else)]

       ([(op :guard b/builtin) & args] :seq)
       [:builtin op (map rd args)]

       (['fn [arg-type
              (arg :guard symbol?)]
         body] :seq)
       (let [read-body (read (assoc ctx arg (inc depth))
                             (inc depth)
                             body)]
         [:fn arg arg-type read-body])

       ([(key :guard keyword?) record] :seq)
       [:lookup key (rd record)]

       ([f arg] :seq)
       [:call
        (rd f)
        (rd arg)]

       (sym :guard symbol?)
       [:var (or (- depth (ctx sym))
                 (throw (ex-info "Var not found"
                                 {:context ctx
                                  :var sym})))]

       (x :guard integer?)
       [:int x]

       (x :guard number?)
       [:number x]

       (b :guard boolean?)
       [:bool b]

       (m :guard map?)
       [:record (map-vals rd m)]))))
