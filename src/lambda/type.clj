(ns lambda.type
  (:require [clojure.core.match :refer [match]]))

(defn from-context [ctx id]
  (second (nth ctx id)))

(defn type-of
  ([term]
   (type-of () term))
  ([ctx term]
   (match
    term

    [:number _ _]
    :Number

    [:var _ id]
    (from-context ctx id)

    [:fn _ arg arg-type body]
    (let [ctx' (cons [arg arg-type] ctx)
          body-type (type-of ctx' body)]
      [:Fn arg-type body-type])

    [:call _ f arg]
    (let [f-type (type-of ctx f)
          arg-type (type-of ctx arg)]
      (match f-type
             [:Fn f-arg-type f-ret-type]
             (if (= arg-type f-arg-type)
               f-ret-type
               (throw (ex-info "Parameter type mismatch" {:expected f-arg-type
                                                          :given arg-type})))

             :else
             (throw (ex-info "Function type expected" {:given f-type})))))))
