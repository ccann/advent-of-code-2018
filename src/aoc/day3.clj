(ns aoc.day3
  (:require [aoc.util :as u]
            [clojure
             [edn :as edn]
             [string :as str]]
            [clojure.java.io :as io]))

(defn parse-line
  [line]
  (let [[s1 _ s3 s4] (str/split line #"\s")
        claim        (second (str/split s1 #"#"))
        [x y _]      (str/split s3 #",|:")
        [w h]        (str/split s4 #"x")]
    (u/map-vals edn/read-string
                {:id    claim
                 :x     x, :y      y
                 :width w, :height h})))

(defn load-claims
  []
  (->> "day3/input.txt"
       io/resource
       slurp
       str/split-lines
       (map parse-line)))

#_(parse-line "#975 @ 83,893: 29x18")

(defn compute-coords
  "e.g. [1 1 2 2] => '([1 1] [1 2] [2 1] [2 2])"
  [{:keys [x y width height]}]
  (let [xs (range x (+ x width))
        ys (range y (+ y height))]
    (mapcat (fn [x] (map (fn [y] [x y]) ys)) xs)))


(defn index-claim
  [pair claim]
  (let [coords (compute-coords claim)]
    (reduce (fn [[m sum] coord]
              (let [v (get-in m coord)]
                (if v
                  (if (= 'X v)
                    ;; 2 or more claims use this coordinate
                    [m sum]
                    ;; one other claim uses this coordinate
                    [(assoc-in m coord 'X) (inc sum)])
                  ;; unused coordinate
                  [(assoc-in m coord (:id claim)) sum])))
            pair
            coords)))


(defn build-graph
  "returns a pair of the graph and the sum total of shared square inches."
  [claims]
  (reduce index-claim [{} 0] claims))


(defn solve-1
  []
  (->> (load-claims)
       (build-graph)
       (second)))


;; -- part 2 --

(defn first-non-overlapping-claim
  [graph claims]
  (some (fn [{:keys [id] :as claim}]
          (let [coords (compute-coords claim)]
            (when (every? (fn [coord] (= id (get-in graph coord)))
                          coords)
              id)))
        claims))

(defn solve-2
  []
  (let [claims (load-claims)
        graph  (->> claims
                    (build-graph)
                    (first))]
    (first-non-overlapping-claim graph claims)))
