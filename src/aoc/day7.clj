(ns aoc.day7
  (:require [clojure
             [set :as set]
             [string :as str]]
            [clojure.java.io :as io]))

(def test-input
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(def input
  (slurp (io/resource "day7/input.txt")))

(defn load-input
  []
  (map (fn [s] (drop 1 (re-find #" (\w) .* (\w) " s)))
       (str/split-lines input)))


(defn add [set v] (if (set? set) (conj set v) #{v}))

(def DAG
  (reduce (fn [m [l r]]
            (-> m
             (update-in [l :children] add r)
             (update-in [r :parents] add l)))
          {}
          (load-input)))

(defn no-parents? [[step m]] (when (empty? (:parents m)) step))

(defn traverse
  [dag]
  (let [[first-step & rem] (sort (map first (filter no-parents? DAG)))]
    (loop [step      first-step
           available rem
           path      []]
      (let [children      (get-in dag [step :children])
            now-available (sort (into available children))
            new-path      (conj path step)
            [next-step]   (filter #(empty? (set/difference (get-in dag [% :parents])
                                                           (set new-path)))
                                  now-available)]
        (println now-available)
        (if (empty? now-available)
          new-path
          (recur next-step
                 (remove #(= next-step %) now-available)
                 new-path))))))

(defn solve [] (apply str (traverse DAG)))

;; -- part 2 --

(defn time-required [step] (+ 60 (- (int (first step)) 64)))

(defn new-job
  [step]
  {:step step
   :time (time-required step)
   :spent 0})

(defn job-done?
  [job]
  (and (some? job)
       (= (:time job) (:spent job))))

;; i'm not proud of this
(defn build-sleigh
  [dag]
  (let [-available    (sort (map first (filter no-parents? dag)))
        done          (atom #{})
        parents-done? #(every? @done (get-in dag [% :parents]))]
    (loop [tick    0
           avail   -available
           workers {0 nil 1 nil 2 nil 3 nil 4 nil}]
      (if (and (empty? avail) (every? nil? (map second workers)))
        tick
        (let [workers        (into {} (for [[w job] workers]
                                        (let [job (when job (update job :spent inc))]
                                          (if (job-done? job)
                                            (do (swap! done conj (:step job))
                                                (println "done!" (:step job))
                                                [w nil])
                                            [w job]))))
              free-workers   (filter (comp nil? second) workers)
              ;; make as many assignments as there are free workers or available steps
              assignments    (->> (filter parents-done? avail)
                                  (sort)
                                  (map new-job)
                                  (map vector (keys free-workers))
                                  (into {}))
              _              (when (seq assignments)
                               (println "tick:" tick "new assignments:" assignments))
              assigned-steps (set (map :step (vals assignments)))
              children       (->> assigned-steps
                                  (map #(get-in dag [% :children]))
                                  (apply set/union)
                                  (vec))
              new-avail      (->> children
                                  (into avail)
                                  (set)
                                  (remove assigned-steps))]
          (when (seq assignments) (println "available:" (sort new-avail)))
          (recur (inc tick)
                 (sort (vec new-avail))
                 (merge workers assignments)))))))


(defn solve-2 [] (dec (build-sleigh DAG)))
