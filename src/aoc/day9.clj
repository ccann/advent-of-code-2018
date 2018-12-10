(ns aoc.day9
  (:import [java.util ArrayList List]))


(def #^java.util.ArrayList marbles
  (ArrayList. [0]))

(defn delete
  [i delta]
  (let [j   (mod (+ i delta) (.size marbles))
        old (.get marbles j)]
    (.remove marbles (int j))
    [j old (.get marbles j)]))

(defn insert
  [i delta v]
  (let [size (.size marbles)
        j    (mod (+ i delta) size)
        j    (if (= j 0) size j)]
    (.add marbles j v)
    [j nil v]))


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
      (next-turn game [ix v] (+ turn old))
      (next-turn game [ix v] 0))))


(defn high-score [game] (apply max (vals (:players game))))

(defn play-game
  [players last-marb-points]
  (let [game {:players (into {} (for [i (range players)] [(inc i) 0]))
              :player     1
              :turn       1
              :marble     [0 0]
              :max-points last-marb-points}]
    (loop [g (do-turn game)]
      (when (= 0 (mod (:turn g) 10000))
        (println (:turn g)))
      (if (<= last-marb-points (:turn g))
        (high-score g)
        (recur (do-turn g))))))

(defn -main [& args]
  (println (play-game 428 (* 100 72061))))


#_(play-game 10 1618)
#_(play-game 13 7999)
#_(play-game 17 1104)
#_(play-game 21 6111)
#_(play-game 30 5807)
#_(play-game 428 (* 100 72061))
