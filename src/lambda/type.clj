(ns lambda.type
  (:require [clojure.core.match :refer [match]]
            [lambda.builtin     :as b]
            [lambda.util        :refer [map-vals]]))

#_(defn format
  ([type] (format () type))
  ([ctx type]
   (let [fmt (partial format ctx)]
     (match type
       [:Record m]
       (map-vals fmt m)

       [:Fn arg-type body-type]
       (str "(" (fmt arg-type) "->" (fmt body-type) ")")

       )))
  )

(defn type-map [on-var depth type]
  (let [walk (fn walk [depth type]
               (let [wlk (partial walk depth)]
                 (match type
                   [:Record m]
                   [:Record (map-vals wlk m)]

                   [:Fn arg-type body-type]
                   [:Fn (wlk arg-type) (wlk body-type)]

                   [:All type-var type-body]
                   [:All type-var (walk (inc depth) type-body)]

                   [:Some type-var type-body]
                   [:Some type-var (walk (inc depth) type-body)]

                   [:Type-Var id]
                   (on-var depth id)

                   :else
                   type)))]
    (walk depth type)))

(defn shift-above [delta depth type]
  (type-map (fn [depth id]
              (if (>= id depth)
                [:Type-Var (+ delta id)]
                [:Type-Var id]))
            depth
            type))

(defn shift [delta type]
  (shift-above delta 0 type))

(defn substitute [var-id replace-type type]
  (type-map (fn [depth id]
              (if (= id depth)
                (shift depth replace-type)
                [:Type-Var id]))
            var-id
            type))

(defn substitute-top [type replace-type]
  (->> type
       (substitute 0 (shift 1 replace-type))
       (shift -1)))

(defn from-context [ctx id]
  (second (nth ctx id)))

(defn type-of
  ([term]
   (type-of () term))
  ([ctx term]
   (println "TYPEOF" ctx term)
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
         [:Fn arg-type (shift -1 body-type)])

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
           (throw (ex-info "Function type expected" {:given f-type}))))

       [:generic type-var body]
       (let [ctx' (cons [type-var :type-var] ctx)
             body-type (type-of ctx' body)]
         [:All type-var body-type])

       [:apply-generic generic type]
       (let [gen-type (typ generic)]
         (match gen-type
           [:All type-var type-body]
           (substitute-top type-body type)

           :else
           (throw (ex-info "Generic type expected" {:given gen-type}))))

       [:pack impl-type body as-type]
       (match as-type
         [:Some type-var type-body]
         (let [body-type (typ body)
               body-type' (substitute-top type-body impl-type)]
           (if (= body-type body-type')
             as-type
             (throw (ex-info "Existential type mismatch" {:expected body-type
                                                          :given body-type'}))))

         :else (throw (ex-info "Existential type expected" {:given as-type})))

       [:unpack type-var var assign-term result-term]
       (let [assign-type (typ assign-term)]
         (match assign-type
           [:Some type-var type-body]
           (let [ctx' (list* [var assign-type]
                             [type-var :type-var]
                             ctx)
                 res-type (type-of ctx' result-term)])

           :else (throw (ex-info "Existential type expected" {:given assign-type}))))))))
