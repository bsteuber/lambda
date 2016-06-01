(ns lambda.term
  (:refer-clojure :exclude [format])
  (:require [clojure.core.match :refer [match]]))

(defn format
  ([term]
   (format () term))
  ([ctx term]
   (match
    term
    [:number _ x]
    x

    [:fn _ arg arg-type body]
    (let [ctx (cons arg ctx)]
      (list 'fn [arg-type
                 arg]
            (format ctx body)))
    [:var _ id]
    (nth ctx id)

    [:call _ f arg]
    (list (format ctx f)
          (format ctx arg)))))

(defn shift [delta term]
  (let [walk (fn walk [depth term]
               (match term
                      [:number _ _]
                      term

                      [:var info id]
                      (if (>= id depth)
                        [:var info (+ delta id)]
                        term)

                      [:fn info arg arg-type body]
                      [:fn info arg arg-type (walk (inc depth) body)]

                      [:call info f arg]
                      [:call info (walk depth f) (walk depth arg)]))]
    (walk 0 term)))

(defn substitute-var [var-id replace-term term]
  (let [walk (fn walk [depth term]
               (match term
                      [:number _ _]
                      term

                      [:var info id]
                      (do
                        (if (= id (+ depth var-id))
                          (shift depth replace-term)
                          term))

                      [:fn info arg arg-type body]
                      [:fn info arg arg-type (walk (inc depth) body)]

                      [:call info f arg]
                      [:call info (walk depth f) (walk depth arg)]))]
    (walk 0 term)))

(defn substitute-top-var [term replace-term]
  (->> term
       (substitute-var 0 (shift 1 replace-term))
       (shift -1)))

(defn value? [ctx term]
  (#{:number :fn} (first term)))
