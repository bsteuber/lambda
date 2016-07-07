(ns lambda.term
  (:refer-clojure :exclude [format])
  (:require [clojure.core.match :refer [match]]
            [lambda.util        :refer [map-vals]]
            [lambda.type        :as ty]))

(defn format
  ([term]
   (format () term))
  ([ctx term]
   (let [fmt (partial format ctx)]
     (match term
       [:number x]
       x

       [:bool x]
       x

       [:record m]
       (map-vals fmt m)

       [:lookup key record]
       (list key record)

       [:if condition then else]
       (list 'if
             (fmt condition)
             (fmt then)
             (fmt else))

       [:fn arg arg-type body]
       (let [ctx' (cons arg ctx)]
         (list 'fn [arg-type
                    arg]
               (format ctx' body)))

       [:var id]
       (nth ctx id)

       [:call f arg]
       (list (fmt f)
             (fmt arg))

       [:builtin op args]
       (cons op
             (map fmt args))

       [:generic type-var body]
       (list 'generic type-var (fmt body))

       [:apply-generic generic type]
       (list 'apply-generic (fmt generic) type)

       [:pack impl-type body as-type]
       (list 'pack impl-type (fmt body) as-type)

       [:unpack type-var var assign-term result-term]
       (list 'let [[type-var var] (fmt assign-term)]
             (fmt result-term))))))

(defn term-map [on-var on-type term]
  (let [walk (fn walk [depth term]
               (let [wlk (partial walk depth)
                     wlk+1 (partial walk (inc depth))
                     wlk+2 (partial walk (+ depth 2))
                     ontyp (partial on-type depth)
                     ontrm (partial on-var depth)]
                 (match term
                   [:var id]
                   (on-var depth id)

                   [:fn arg arg-type body]
                   [:fn arg (ontyp arg-type) (wlk+1 body)]

                   [:call f arg]
                   [:call (wlk f) (wlk arg)]

                   [:builtin op args]
                   [:builtin op (map wlk args)]

                   [:if condition then else]
                   [:if (wlk condition)
                    (wlk then)
                    (wlk else)]

                   [:record m]
                   [:record (map-vals wlk m)]

                   [:lookup key record]
                   [:lookup key (wlk record)]

                   [:generic type-arg body]
                   [:generic type-arg (wlk+1 body)]

                   [:apply-generic generic type-arg]
                   [:apply-generic (wlk generic) (ontyp type-arg)]

                   [:pack impl-type body as-type]
                   [:pack (ontyp impl-type) (wlk body) (ontyp as-type)]

                   [:unpack type-var var assign-term result-term]
                   [:unpack type-var var (wlk assign-term) (wlk+2 result-term)]

                   :else
                   term)))]
    (walk 0 term)))

(defn shift [delta term]
  (term-map (fn [depth id]
              (if (>= id depth)
                [:var (+ delta id)]
                [:var id]))
            (partial ty/shift-above delta)
            term))

(defn substitute-var [var-id replace-term term]
  (term-map (fn [depth id]
              (if (= id (+ depth var-id))
                (shift depth replace-term)
                [:var id]))
            (fn [depth type]
              type)
            term))

(defn substitute-type-var-0 [replace-type term]
  (term-map (fn [depth id]
              [:var id])
            (fn [depth type]
              (ty/substitute depth replace-type type))
            term))

(defn substitute-top-var [term replace-term]
  (->> term
       (substitute-var 0 (shift 1 replace-term))
       (shift -1)))

(defn substitute-top-type-var [term replace-type]
  (->> term
       (substitute-type-var-0 (ty/shift 1 replace-type))
       (shift -1)))

(defn value? [ctx [tag & args]]
  (or (#{:bool :number :fn :generic :pack} tag)
      (and (= tag :record)
           (every? (partial value? ctx)
                   (vals (first args))))))
