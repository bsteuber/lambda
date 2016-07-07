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
   (println "READ" ctx depth expr)
   (let [rd (partial read ctx depth)]
     (match expr
       (['generic (type-var :guard symbol?) body] :seq)
       [:generic type-var (rd body)]

       (['apply-generic generic type] :seq)
       [:apply-generic (rd generic) type]

       (['pack impl-type body as-type] :seq)
       (let [read-body (read (assoc ctx impl-type (inc depth))
                             (inc depth)
                             body)]
         [:pack impl-type read-body as-type])

       (['let [[type-var var] assign-term]
         result-term] :seq)
       (let [read-body (read (assoc ctx var (inc depth))
                             (inc depth)
                             result-term)]
         [:unpack type-var var (rd assign-term) read-body])

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

       (x :guard number?)
       [:number x]

       (b :guard boolean?)
       [:bool b]

       (m :guard map?)
       [:record (map-vals rd m)]))))
