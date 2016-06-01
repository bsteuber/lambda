(ns lambda.reader
  (:refer-clojure :exclude [read])
  (:require [clojure.core.match :refer [match]]))

(defn read
  ([expr]
   (read {} 0 expr))
  ([ctx depth expr]
   (match
    expr

    (['fn [(arg :guard symbol?)]
      body] :seq)
    (let [arg-type (first (keys (meta arg)))
          read-body (read (assoc ctx arg (inc depth))
                          (inc depth)
                          body)]
      [:fn nil arg arg-type read-body])

    ([f arg] :seq)
    [:call nil
     (read ctx depth f)
     (read ctx depth arg)]

    (sym :guard symbol?) [:var nil (or (- depth (ctx sym))
                                     (throw (ex-info "Var not found"
                                                     {:context ctx
                                                      :var sym})))]

    (x :guard number?) [:number nil x])))
