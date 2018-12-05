(ns aoc.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn react?
  [c1 c2]
  (let [c1 (int c1)
        c2 (int c2)]
    (or (= c1 (+ 32 c2))
        (= c2 (+ 32 c1)))))

(defn load-input []
  (-> "day5/input.txt"
      io/resource
      slurp
      str/trim-newline
      char-array
      vec))


(defn react
  [polymer]
  (loop [[c1 c2 & rem :as cs] polymer
         acc                  []
         n                    0]
    (cond (empty? cs) [acc n]
          (nil? c2)   [(conj acc c1) n]
          :else
          (if (react? c1 c2)
            (recur rem acc (inc n))
            (recur (drop 1 cs) (conj acc c1) n)))))


(defn fully-react
  [& [polymer]]
  (loop [polymer (or polymer (load-input))]
    (let [[cs n] (react polymer)]
      (if (pos? n)
        (recur cs)
        cs))))

#_(solve (vec (char-array "dabAcCaCBAcCcaDA")))
#_(solve (vec (char-array "aaAbbBcCccccceefEEe")))

(defn solve
  []
  (count (fully-react)))


;; -- part 2 --

(defn evaluate-units
  [polymer]
  (let [char-set        (sort-by int (set polymer))
        [uppers lowers] (split-at (/ (count char-set) 2) char-set)
        units           (map set (map vector uppers lowers))]
    (for [unit units]
      (do (println "remove" unit)
          (let [new-polymer (remove unit polymer)]
            [unit (count (fully-react new-polymer))])))))

(defn solve-2
  []
  (->> (load-input)
       evaluate-units
       (sort-by second)
       first))
