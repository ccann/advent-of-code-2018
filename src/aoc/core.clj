(ns aoc.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; -- day 1 --

;; -- pretty simple solution, could have done something streamier.
(defn load-input
  []
  (map #(Long. %)
       (str/split (slurp (io/resource "day1/input.txt"))
                  #"\s")))

(defn freq
  [freqs]
  (reduce + 0 freqs))

#_(freq (load-input))

;;  -- part 2 --

(defn first-rep-freq
  [& [input]]
  (loop [deltas (or input (load-input))
         freq   0
         seen   #{0}]
    (if-let [delta (first deltas)]
      (let [new-freq (+ freq delta)]
        (if (seen new-freq)
          new-freq
          (recur (next deltas)
                 new-freq
                 (conj seen new-freq))))
      (recur (or input (load-input))
               freq
               seen))))


#_(assert (= 0 (first-rep-freq [1 -1])))
#_(assert (= 10 (first-rep-freq [3 3 4 -2 -4])))
#_(assert (= 5 (first-rep-freq [-6 3 8 5 -6])))
#_(assert (= 14 (first-rep-freq [7 7 -2 -7 -4])))
#_(assert (= 2 (first-rep-freq [1 -2 3 1 1 -2])))
#_(first-rep-freq)
;; answer: 56360
