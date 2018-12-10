(ns aoc.day9)

;; (def input [428 72061])


(defprotocol Circular
  (insert [this i delta v])
  (delete [this i delta]))

(defn -insert
  [marbles i delta v]
  (let [j         (mod (+ i delta) (count marbles))
        [bef aft] (split-at j marbles)
        marbs     (concat bef [v] aft)
        ret       (if (= j 0)
                    [(count marbles) v (conj (vec marbles) v)]
                    [j v marbs])]
    ret))

(defn -delete
  [marbles i delta]
  (let [j         (mod (+ i delta) (count marbles))
        [bef aft] (split-at j marbles)
        marbs     (concat bef (rest aft))]
    [j (nth marbles j) marbs]))

(defrecord Board [marbles]
  Circular
  (insert [this i delta v] (-insert marbles i delta v))
  (delete [this i delta] (-delete marbles i delta)))


#_(def b (Board. [0 2 1]))


#_(insert b 1 2 3)


(defn next-turn
  [game marbles current-marble score]
  (-> game
      (update :player #(inc (mod % (count (keys (:players game))))))
      (update :turn inc)
      (update-in [:players (:player game)] #(+ % score))
      (assoc :board (Board. marbles))
      (assoc :marble current-marble)))


(defn do-turn
  [game]
  (let [{:keys [turn marble]} game
        [curr-ix _]           marble
        turn-23?              (zero? (mod turn 23))
        action                (if turn-23?
                                #(delete % curr-ix -7)
                                #(insert % curr-ix 2 turn))
        [ix v marbs]          (action (:board game))]
    ;; (clojure.pprint/pprint marbs)
    (if turn-23?
      (next-turn game marbs [ix (nth marbs ix)] (+ turn v))
      (next-turn game marbs [ix v] 0))))


(defn high-score [game] (apply max (vals (:players game))))

(defn play-game
  [players last-marb-points]
  (let [game {:players (into {} (for [i (range players)] [(inc i) 0]))
              :player     1
              :turn       1
              :board      (Board. [0])
              :marble     [0 0]
              :max-points last-marb-points}]
    (loop [g (do-turn game)]
      (println (:turn g))
      (if (<= last-marb-points (:turn g))
        (high-score g)
        (recur (do-turn g))))))



#_(play-game 10 1618)
#_(play-game 13 7999)
#_(play-game 17 1104)
#_(play-game 21 6111)
#_(play-game 30 5807)
(play-game 428 72061)
