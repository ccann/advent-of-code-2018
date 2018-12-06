(ns aoc.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.util :as u]))

(defn load-input
  []
  (->> "day6/input.txt"
       io/resource
       slurp
       str/split-lines
       (map #(str/split % #", "))
       (map #(mapv edn/read-string %))))


(defn make-grid
  [coords]
  (let [[[min-x _]] (sort-by first coords)
        [[max-x _]] (sort-by first > coords)
        [[_ min-y]] (sort-by second coords)
        [[_ max-y]] (sort-by second > coords)
        xs          (range min-x (inc max-x))
        ys          (range min-y (inc max-y))
        locations   (for [x xs y ys] [x y])
        grid        (reduce (fn [m [x y]]
                              (assoc-in m [x y] nil))
                            {} locations)]
    [locations grid [min-x max-x min-y max-y]]))


(defn man-dist
  [x1 y1 x2 y2]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))


(defn mark-locations
  [grid coords locations]
  (reduce (fn [m [x y]]
            (let [dists (sort-by second (map (fn [[cx cy :as coord]]
                                               [coord (man-dist x y cx cy)])
                                             coords))
                  coord (-> dists first first)]
              (if (apply distinct? (take 2 (map second dists)))
                (assoc-in m [x y] coord)
                m)))
          grid locations))


(defn any-on-edge?
  [[c locs] [min-x max-x min-y max-y]]
  (if c
    (some (fn [[_ [x y]]]
            (or (= x max-x) (= y max-y)
                (= x min-x) (= y min-y)))
          locs)
    true))

(defn areas
  [grid corners]
  (->> (for [[x ys] grid, [y coord] ys]
         [coord [x y]])
       (group-by first)
       (remove #(any-on-edge? % corners))
       (into {})
       (u/map-vals count)))


(defn largest-area
  []
  (let [coords              (load-input)
        [locs grid corners] (make-grid coords)
        grid                (mark-locations grid coords locs)
        grid-areas-map      (areas grid corners)]
    (first (sort-by second > grid-areas-map))))


(defn solve
  []
  (let [[coord area] (largest-area)]
    area))


;; -- part 2 --

(defn within-region?
  [[x y] coords]
  (let [dists      (map (fn [[cx cy]] (man-dist x y cx cy)) coords)
        total-dist (apply + dists)]
    (< total-dist 10000)))

(defn region-area
  []
  (let [coords      (load-input)
        [locs & _]  (make-grid coords)
        region-locs (filter (fn [loc] (within-region? loc coords)) locs)]
    (count region-locs)))


(defn solve-2
  []
  (region-area))
