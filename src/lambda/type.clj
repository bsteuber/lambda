(ns lambda.type
  (:require [clojure.core.match :refer [match]]
            [lambda.builtin     :as b]
            [lambda.util        :refer [map-vals]]))

(defn from-context [ctx id]
  (second (nth ctx id)))

(defn type-map [f type]
  (let [walk (fn walk [depth type]
               (let [wlk (partial walk depth)]
                 (match type
                   [:Record m]
                   [:Record (map-vals wlk m)]

                   [:Fn arg-type body-type]
                   [:Fn (wlk arg-type) (wlk body-type)]

                   [:Forall type-var type-body]
                   [:Forall type-var (walk (inc depth) type-body)]

                   [:Exists type-var type-body]
                   [:Exists type-var (walk (inc depth) type-body)]

                   [:Type-Var id]
                   (f depth id)

                   :else
                   type)))]
    (walk 0 type)))

(defn shift [delta type]
  (type-map (fn [depth id]
              (if (>= id depth)
                [:Type-Var (+ delta id)]
                [:Type-Var id]))
            type))

(defn substitute-var [var-id replace-type type]
  (type-map (fn [depth id]
              (if (= id (+ depth var-id))
                (shift depth replace-type)
                [:Type-Var id]))
            type))

(defn substitute-top-var [type replace-type]
  (->> type
       (substitute-var 0 (shift 1 replace-type))
       (shift -1)))

(defn type-of
  ([term]
   (type-of () term))
  ([ctx term]
   (let [typ (partial type-of ctx)]
     (match
       term

       [:bool _]
       :Bool

       [:number _]
       :Number

       [:record m]
       [:Record (map-vals typ m)]

       [:lookup key record]
       (let [record-type (typ record)]
         (match record-type
           [:Record m]
           (or (key m)
               (throw (ex-info "Illegal lookup key for record type"
                               {:key key
                                :record-type record-type})))
           :else
           (throw (ex-info "Record-type expected"
                           {:key key
                            :record-type record-type}))))

       [:builtin op args]
       (let [arg-types (map typ args)
             op-arg-types (b/arg-types op)]
         (if (= op-arg-types arg-types)
           (b/result-type op)
           (throw (ex-info "Wrong builtin arg types"
                           {:operator op
                            :expected op-arg-types
                            :given arg-types}))))

       [:if condition then else]
       (let [cond-type (typ condition)
             then-type (typ then)
             else-type (typ else)]
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
       (let [f-type (typ f)
             arg-type (typ arg)]
         (match f-type
           [:Fn f-arg-type f-ret-type]
           (if (= arg-type f-arg-type)
             f-ret-type
             (throw (ex-info "Parameter type mismatch" {:expected f-arg-type
                                                        :given arg-type})))

           :else
           (throw (ex-info "Function type expected" {:given f-type}))))))))
