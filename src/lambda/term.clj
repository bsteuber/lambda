(ns lambda.term
  (:refer-clojure :exclude [format])
  (:require [clojure.core.match :refer [match]]
            [lambda.util        :refer [map-vals]]))

(defn format
  ([term]
   (format () term))
  ([ctx term]
   (let [fmt (partial format ctx)]
     (match
       term
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
             (map fmt args))))))

(defn term-map [f term]
  (let [walk (fn walk [depth term]
               (let [wlk (partial walk depth)]
                 (match term
                   [:var id]
                   (f depth id)

                   [:fn arg arg-type body]
                   [:fn arg arg-type (walk (inc depth) body)]

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

                   :else
                   term)))]
    (walk 0 term)))

(defn shift [delta term]
  (term-map (fn [depth id]
              (if (>= id depth)
                [:var (+ delta id)]
                [:var id]))
            term))

(defn substitute-var [var-id replace-term term]
  (term-map (fn [depth id]
              (if (= id (+ depth var-id))
                (shift depth replace-term)
                [:var id]))
            term))

(defn substitute-top-var [term replace-term]
  (->> term
       (substitute-var 0 (shift 1 replace-term))
       (shift -1)))

(defn value? [ctx [tag & args]]
  (or (#{:bool :number :fn} tag)
      (and (= tag :record)
           (every? (partial value? ctx)
                   (vals (first args))))))
