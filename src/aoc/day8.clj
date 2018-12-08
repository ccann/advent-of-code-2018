(ns aoc.day8
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))


(def test-input
  (map edn/read-string (str/split "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" #" ")))

(def input
  (-> "day8/input.txt"
       io/resource
       slurp
       str/trim-newline
       (str/split #" ")
       (->> (map #(Long. %)))))

(defn index-node
  [m node [childs metas]]
  (assoc m node {:childs-remaining childs :metas-remaining metas
                 :values []
                 :childs childs}))

(defn childs-rem?
  [m node]
  (pos? (get-in m [node :childs-remaining])))

(defn a-parent?
  [m node]
  (pos? (get-in m [node :childs])))

(defn dec-childs
  [m node]
  (update-in m [node :childs-remaining] dec))

(defn metas-rem
  [m node]
  (get-in m [node :metas-remaining]))

;; `node` tracks the depth of the node in the tree.
;; while over the course of the algorithm multiple nodes will have the same depth, they
;; never do so at the same time.
;; Decrementing a node gets you its parent.

(defn meta-entries
  [numbers]
  (loop [ns numbers, node 1, m {}, entries []]
    (if (empty? ns)
      entries
      ;; index the node if it's unindexed
      (let [[header rem] (split-at 2 ns)
            [ns m]       (if (get m node)
                           [ns m]
                           [rem (index-node m node header)])]
        (if (childs-rem? m node)
          (recur ns (inc node) (dec-childs m node) entries)
          ;; accumulate metadata entries
          (let [[es ns] (split-at (metas-rem m node) ns)]
            (recur ns (dec node) (dissoc m node) (into entries es))))))))


(defn solve [] (apply + (meta-entries input)))

;; -- part 2 --

(defn add-val-to-parent
  "Add this node's value to its parent's index."
  [m node value]
  (-> m
      (update-in [(dec node) :values] conj value)
      (dissoc node)))


(defn node-value
  [m node indexes]
  (let [is (remove neg? (map dec indexes))
        vs (get-in m [node :values])]
    (reduce (fn [n i] (+ n (get vs i 0)))
            0 is)))


(defn meta-values
  [numbers]
  (loop [ns numbers, node 1, m {}]
    (let [[header rem] (split-at 2 ns)
          [ns m]       (if (get m node)
                         [ns m]
                         [rem (index-node m node header)])]
      (if (childs-rem? m node)
        (recur ns (inc node) (dec-childs m node))
        ;; if ever had children, use indexes
        ;; if never had children, sum
        (let [[es ns] (split-at (metas-rem m node) ns)
              value   (if (a-parent? m node) (node-value m node es) (apply + es))]
          (if (empty? ns)
            (node-value m node es)
            (recur ns (dec node) (add-val-to-parent m node value))))))))


(defn solve-2 [] (meta-values input))
