(ns lambda.term
  (:refer-clojure :exclude [format])
  (:require [clojure.core.match :refer [match]]))

(defn format
  ([term]
   (format () term))
  ([ctx term]
   (match
    term
    [:number x]
    x

    [:bool x]
    x

    [:if condition then else]
    (list 'if
          (format ctx condition)
          (format ctx then)
          (format ctx else))

    [:fn arg arg-type body]
    (let [ctx (cons arg ctx)]
      (list 'fn [arg-type
                 arg]
            (format ctx body)))
    [:var id]
    (nth ctx id)

    [:call f arg]
    (list (format ctx f)
          (format ctx arg)))))

(defn shift [delta term]
  (let [walk (fn walk [depth term]
               (match term
                      [:var id]
                      (if (>= id depth)
                        [:var (+ delta id)]
                        term)

                      [:fn arg arg-type body]
                      [:fn arg arg-type (walk (inc depth) body)]

                      [:call f arg]
                      [:call (walk depth f) (walk depth arg)]

                      [:if condition then else]
                      [:if (walk depth condition)
                       (walk depth then)
                       (walk depth else)]

                      :else
                      term))]
    (walk 0 term)))

(defn substitute-var [var-id replace-term term]
  (let [walk (fn walk [depth term]
               (match term
                      [:var id]
                      (do
                        (if (= id (+ depth var-id))
                          (shift depth replace-term)
                          term))

                      [:fn arg arg-type body]
                      [:fn arg arg-type (walk (inc depth) body)]

                      [:call f arg]
                      [:call (walk depth f) (walk depth arg)]

                      [:if condition then else]
                      [:if (walk depth condition)
                       (walk depth then)
                       (walk depth else)]

                      :else
                      term))]
    (walk 0 term)))

(defn substitute-top-var [term replace-term]
  (->> term
       (substitute-var 0 (shift 1 replace-term))
       (shift -1)))

(defn value? [ctx term]
  (#{:bool :number :fn} (first term)))
