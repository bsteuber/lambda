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

    [:bool _]
    :Bool

    [:number _]
    :Number

    [:if condition then else]
    (let [cond-type (type-of ctx condition)
          then-type (type-of ctx then)
          else-type (type-of ctx else)]
      (if (= :Bool cond-type)
        (if (= then-type else-type)
          then-type
          (throw (ex-info "if: then and else branches have different types"
                          {:then then
                           :else else
                           :then-type then-type
                           :else-type else-type})))
        (throw (ex-info "if: condition must have type :Bool"
                        {:condition condition
                         :condition-type cond-type}))))

    [:var id]
    (from-context ctx id)

    [:fn arg arg-type body]
    (let [ctx' (cons [arg arg-type] ctx)
          body-type (type-of ctx' body)]
      [:Fn arg-type body-type])

    [:call f arg]
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
