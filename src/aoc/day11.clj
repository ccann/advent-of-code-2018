(ns aoc.day11)

(def dim-x 300)
(def dim-y 300)

(defn make-grid
  []
  (into-array (map int-array (repeat dim-x (repeat dim-y 0)))))


(def ^long serial-number 5535)

(defn keep-hundreds
  [n]
  (if (< 99 n)
    (Long. (str (nth (reverse (str n)) 2)))
    0))


(defn power-level
  [x y serial]
  (let [rack-id (+ x 10)
        power   (-> (* rack-id y)
                    (+ serial)
                    (* rack-id)
                    (keep-hundreds)
                    (- 5))]
    power))


(defn set-power-levels
  [grid]
  (doseq [x (range dim-x) y (range dim-y)]
    (let [[-x -y] [(inc x) (inc y)]
          power   (power-level -x -y serial-number)]
      (aset grid x y (int power)))))


(def master-grid (make-grid))
#_(set-power-levels master-grid)

(assert (= 4 (power-level 3 5 8)))
(assert (= -5 (power-level 122 79 57)))
(assert (= 0 (power-level 217 196 39)))
(assert (= 4 (power-level 101 153 71)))


;; part 2

(defn -power [grid x y] (aget grid x y))
(def power (memoize -power))

(defn -rng [start end] (vec (range start end)))
(def rng (memoize -rng))

(def x-range (range dim-x))
(def y-range (range dim-y))

(declare square-power)

(defn -square-power
  [size x y]
  (let [y-max (+ y size)
        x-max (+ x size)
        xs    (rng x x-max)
        ys    (rng y y-max)]
    (cond
      (= 2 size)
      (apply + (for [-x xs, -y ys] (power master-grid -x -y)))

      (even? size)
      (let [-x   (first xs)
            -y   (first ys)
            mid  (/ size 2)
            midy (nth ys mid)
            midx (nth xs mid)
            pts  [[-x -y] [midx -y] [-x midy] [midx midy]]]
        (apply + (mapv #(square-power mid (first %) (second %)) pts)))

      :else
      (let [inner-square-pwr (square-power (dec size) (first xs) (first ys))
            bottom-row-pwr   (apply + (for [y (butlast ys)]
                                        (power-level master-grid (last xs) y)))
            right-col-pwr    (apply + (for [x xs]
                                        (power-level master-grid x (last ys))))]
        (+ inner-square-pwr
           right-col-pwr
           bottom-row-pwr)))))

(def square-power (memoize -square-power))

(defn square-power-levels
  [size levels]
  (println "size" size)
  (time
   (doseq [x x-range y y-range]
     (let [y-max (+ y size) x-max (+ x size)]
       (when (and (<= x-max dim-x) (<= y-max dim-y))
         (conj! levels [[(inc x) (inc y) size] (square-power size x y)]))))))


(set! *unchecked-math* true)

(defn solve-2
  []
  (time (let [sizes  (vec (range 2 301))
              levels (transient [])]
          (mapv #(square-power-levels % levels) sizes)
          (println (first (sort-by second > (persistent! levels)))))))
