(ns aoc.day2
  (:require [clojure
             [data :as data]
             [edn :as edn]
             [string :as str]]
            [clojure.java.io :as io]))

(defn load-box-ids
  []
  (->> "day2/input.txt"
       io/resource
       slurp
       str/split-lines))

(defn checksum
  [ids]
  (let [freqs (map (comp set vals frequencies) ids)
        n     (count (filter #(contains? % 2) freqs))
        m     (count (filter #(contains? % 3) freqs))]
    (* n m)))

#_(= 0 (checksum ["abcdef"]))
#_(= 0 (checksum ["ababab"]))
#_(= 1 (checksum ["bababc"]))
#_(checksum (load-box-ids))


;; -- part 2 --

(defn close-match
  [id1 id2]
  (let [[_ _ both] (clojure.data/diff (vec id1) (vec id2))]
    (when (and (= (count id1) (count both))
               (= 1 (count (filter nil? both))))
      both)))


;; could use reduce + reduced here to terminate search early

(defn common-chars
  [box-ids]
  (->> box-ids
       (map-indexed (fn [i id]
                      (let [rest-ids (drop (inc i) box-ids)]
                        (->> rest-ids
                             (map #(close-match % id))
                             (remove nil?)
                             first))))
       (remove nil?)
       (first)))


(defn solve
  []
  (->> (load-box-ids)
       (common-chars)
       (apply str)))

#_(solve)
