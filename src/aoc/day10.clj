(ns aoc.day10
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import java.util.ArrayList))

(def input "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")

(def xs 300)
(def ys 300)
(def diff 0)

(defn parse
  [line]
  (let [[x y dx dy] (map edn/read-string (re-seq #"[0-9-]+" line))
        [x y] [(+ x diff) (+ y diff)]]
    {:curr [x y] :vel [dx dy]}))

#_(defn load-input []
    (map parse (str/split-lines input)))

(defn load-input []
  (map parse (str/split-lines (slurp (io/resource "day10/input.txt")))))


(defn make-graph
  [points]
  (to-array-2d (repeat xs (repeat ys "."))))


(defn display
  [graph min-x min-y]
  (remove
   (fn [row] (every? #(= \. %) row))
   (for [x (range min-x xs)]
     (reduce
      (fn [s i] (str s (try (aget graph i x) (catch Exception e "-"))))
      ""
      (range min-y ys)))))


(defn man-dist
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn converged?
  [pt pts]
  (some (fn [{:keys [curr]}]
          (when-not (= curr (:curr pt))
            (<= (man-dist curr (:curr pt)) 2)))
        pts))

(def points (atom (load-input)))

(defn tick
  [graph & [one?]]
  (loop [pts @points i 0]
    (println "tick" i)
    (let [[ps _] (reduce (fn [[v newset] {:keys [curr vel] :as m}]
                           (let [[x y]         curr
                                 [dx dy]       vel
                                 [new-x new-y] [(+ x dx) (+ y dy)]]
                             (when-not (newset curr)
                               (try (aset graph x y ".") (catch Exception e nil)))
                             (try (aset graph new-x new-y "#") (catch Exception e nil))
                             [(conj! v (assoc m :curr [new-x new-y]))
                              (conj! newset [new-x new-y])]))
                         [(transient []) (transient #{})]
                         pts)
          ps     (persistent! ps)]
      (if (some #(neg? (first (:curr %))) ps)
        (recur ps (inc i))
        (if (every? #(converged? % ps) ps)
          (reset! points ps)
          (recur ps (inc i)))))))


(def g (make-graph @points))
(tick g)
(display g 0 0)

(set! *print-length* 100000)
