(ns lambda.util)

(defn map-vals [f m]
  (->> m
       (map (fn [[k v]]
              [k (f v)]))
       (into {})))
