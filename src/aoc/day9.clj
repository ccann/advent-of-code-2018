(ns aoc.day9
  (:import [java.util ArrayList List]))

;; (def input [428 72061])



#_(defrecord Board [marbles]
  Circular
  (insert [this i delta v]
    (let [size      (count marbles)
          j         (mod (+ i delta) size)
          [bef aft] (split-at j marbles)
          marbs     (concat bef [v] aft)
          ret       (if (= j 0)
                      [size v (conj (vec marbles) v)]
                      [j v marbs])]
      ret))
  (delete [this i delta]
    (let [j         (mod (+ i delta) (count marbles))
          [bef aft] (split-at j marbles)
          marbs     (concat bef (rest aft))]
      [j (nth marbles j) marbs])))

(def #^java.util.ArrayList marbles
  (ArrayList. [0]))

(defn delete
  [i delta]
  (let [j   (mod (+ i delta) (.size marbles))
        old (.get marbles j)]
    (.remove marbles (int j))
    ;; (println marbles)
    [j old (.get marbles j)]))

(defn insert
  [i delta v]
  (let [size (.size marbles)
        j    (mod (+ i delta) size)
        j    (if (= j 0) size j)]
    (.add marbles j v)
    ;; (println marbles)
    [j nil v]))



;; (def #^java.util.ArrayList temp (java.util.ArrayList. ["foo"]))
;; (defn foo []
;;   (let [i (mod 3 3)]
;;     (.get temp 0)
;;     (.remove temp (int i))))


;; marbles
#_(insert 0 2 1)
#_(insert 1 2 2)
#_(insert 1 2 3)
#_(insert 3 2 4)
;; (delete 3 -2)



(defn next-turn
  [game current-marble score]
  (-> game
      (update :player #(inc (mod % (count (keys (:players game))))))
      (update :turn inc)
      (update-in [:players (:player game)] #(+ % score))
      (assoc :marble current-marble)))


(defn do-turn
  [game]
  (let [{:keys [turn marble]} game
        [curr-ix _]           marble
        turn-23?              (zero? (mod turn 23))
        action                (if turn-23?
                                #(delete curr-ix -7)
                                #(insert curr-ix 2 turn))
        [ix old v]            (action)]
    (if turn-23?
      (do #_(println (:turn game) "score" (+ turn old))
          (next-turn game [ix v] (+ turn old)))
      (next-turn game [ix v] 0))))


(defn high-score [game] (apply max (vals (:players game))))

(defn play-game
  [players last-marb-points]
  (let [game {:players (into {} (for [i (range players)] [(inc i) 0]))
              :player     1
              :turn       1
              ;; :board      (doto (ArrayList.) (.add 0))
              :marble     [0 0]
              :max-points last-marb-points}]
    (loop [g (do-turn game)]
      (when (= 0 (mod (:turn g) 10000))
        (println (:turn g)))
      ;; (println (:turn g))
      ;; (println (:turn g) (vec marbles))
      (if (<= last-marb-points (:turn g))
        (high-score g)
        (recur (do-turn g))))))



#_(play-game 10 1618)
#_(play-game 13 7999)
#_(play-game 17 1104)
#_(play-game 21 6111)
#_(play-game 30 5807)
#_(play-game 428 (* 100 72061))
