(ns aoc.day4
  (:require [clj-time
             [core :as t]
             [format :as tf]]
            [clojure
             [edn :as edn]
             [string :as str]]
            [clojure.java.io :as io]))

(defn- parse-date-str
  [s]
  (tf/parse (tf/formatter "yyyy-MM-dd HH:mm")
            (second (re-find #"\[(.*)\]" s))))


(defn load-input
  []
  (->> (io/resource "day4/input.txt")
       (slurp)
       (str/split-lines)
       (sort-by parse-date-str)))


(defn sleep-vectors
  [lines]
  (for [[sleep wake] (partition 2 lines)]
    (let [sleep-time (parse-date-str sleep)
          wake-time  (parse-date-str wake)]
      (vec (range (t/minute sleep-time) (t/minute wake-time))))))


(defn- get-id [s] (re-find #"(?<=#)[0-9]+" s))

(defn guards
  "Create a map of guard id to sleep vectors"
  [sorted-lines]
  (loop [lines  sorted-lines
         guards {}]
    (if (empty? lines)
      guards
      (let [[s & -lines] lines
            no-id?       (comp not get-id)
            id           (edn/read-string (get-id s))
            events       (take-while no-id? -lines)
            sleep-vecs   (sleep-vectors events)]
        (recur (drop-while no-id? -lines)
               (if (get guards id)
                 (update guards id into sleep-vecs)
                 (assoc guards id sleep-vecs)))))))

(defn sleepiest-guard
  [guards]
  (let [mins-asleep #(->> % val flatten count)
        sleepiest   (first (sort-by mins-asleep > guards))]
    sleepiest))


(defn sleepiest-minute
  [guard]
  (first (sort-by second > (frequencies (flatten (val guard))))))


(defn solve
  []
  (let [ms                   (guards (load-input))
        [id _ :as sleepiest] (sleepiest-guard ms)
        [minute _]           (sleepiest-minute sleepiest)]
    (* id minute)))



;; -- part 2 --

(defn solve-2
  []
  (let [ms                  (guards (load-input))
        pairs               (map vector ms (map sleepiest-minute ms))
        [[id _] [minute _]] (first (sort-by #(or (-> % second second) 0) > pairs))]
    (* id minute)))
