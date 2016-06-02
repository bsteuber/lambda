(ns lambda.type
  (:refer-clojure :exclude [>])
  (:require [clojure.core.match :refer [match]]
            [lambda.builtin     :as b]
            [lambda.util        :refer [map-vals]]))

(defn from-context [ctx id]
  (second (nth ctx id)))

(defn > [super-type sub-type]
  (or (= super-type sub-type)
      (match [super-type sub-type]
        [:Top _]
        true

        [[:Fn super-arg super-res]
         [:Fn sub-arg sub-res]]
        (and (> sub-arg super-arg)
             (> super-res sub-res))

        [[:Record super-m]
         [:Record sub-m]]
        (every? (fn [[key super-type]]
                  (when-let [sub-type (key sub-m)]
                    (> super-type sub-type)))
                super-m)

        [:Number :Int]
        true

        :else
        false)))

(declare intersection)

(defn union [type-1 type-2]
  (cond
    (= type-1 type-2)
    type-1

    (> type-1 type-2)
    type-1

    (> type-2 type-1)
    type-2

    :else
    (match [type-1 type-2]
      [[:Fn arg-1 res-1] [:Fn arg-2 res-2]]
      [:Fn (intersection arg-1 arg-2) (union res-1 res-2)]

      [[:Record m-1] [:Record m-2]]
      [:Record (->> m-1
                    (map (fn [[key type-1]]
                           (when-let [type-2 (key m-2)]
                             [key (union type-1 type-2)])))
                    (remove nil?)
                    (into {}))]

      :else
      :Top)))

(defn intersection [type-1 type-2]
  (cond
    (= type-1 type-2)
    type-1

    (> type-1 type-2)
    type-2

    (> type-2 type-1)
    type-1

    :else
    (do
      (match [type-1 type-2]
        [[:Fn arg-1 res-1] [:Fn arg-2 res-2]]
        [:Fn (union arg-1 arg-2) (intersection res-1 res-2)]

        [[:Record m-1] [:Record m-2]]
        [:Record (->> (concat (keys m-1) (keys m-2))
                      distinct
                      (map (fn [key]
                             [key
                              (match [(key m-1) (key m-2)]
                                [ty-1 nil] ty-1
                                [nil ty-2] ty-2
                                [ty-1 ty-2] (intersection ty-1 ty-2))]))
                      (into {}))]

        :else
        (throw (ex-info "No intersection type"
                        {:type-1 type-1
                         :type-2 type-2}))))))

(defn type-of
  ([term]
   (type-of () term))
  ([ctx term]
   (let [typ (partial type-of ctx)]
     (match
       term

       [:bool _]
       :Bool

       [:int _]
       :Int

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
         (if (every? identity
                     (map > op-arg-types arg-types))
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
           (if-let [result-type (union then-type else-type)]
             result-type
             (throw (ex-info "if: then and else branches have no union type"
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
           (if (> f-arg-type arg-type)
             f-ret-type
             (throw (ex-info "Parameter type mismatch" {:expected f-arg-type
                                                        :given arg-type})))

           :else
           (throw (ex-info "Function type expected" {:given f-type}))))))))
