(ns aoc.util)

(defn map-vals
  [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn filter-vals
  [pred m]
  (into {} (remove nil? (map (fn [[k v]] (when (pred v)
                                           [k v]))
                             m))))
