(ns aoc-2022.day24
  (:require
   [clojure.string :as str]))

(def cardinals [[0 0] [-1 0] [0 1] [1 0] [0 -1]])

(def wind-directions
  {\^ [-1 0]
   \> [0 1]
   \v [1 0]
   \< [0 -1]})

(defn v+ [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn parse [input]
  (let [grid (str/split-lines input)
        rows (count grid)
        cols (count (first grid))]
    (->>
     (for [row (range rows)
           col (range cols)]
       (let [pos [row col]
             ch  (get-in grid [row col])]
         (case ch
           \#            {:wall? #{pos}}
           (\^ \> \v \<) {:blizzards [{:pos pos :type ch}]}
           \.            nil)))
     (apply merge-with #(apply conj %1 %2))
     (merge {:rows rows :cols cols}))))

(defn blow [{:keys [rows cols]} {:keys [pos type]}]
  (let [[r c] (v+ pos (wind-directions type))]
    {:type type
     :pos  [(inc (mod (dec r) (- rows 2)))
            (inc (mod (dec c) (- cols 2)))]}))

(defn tick [{:keys [wall? blizzards rows cols] :as state}]
  (let [blizzards   (mapv #(blow state %) blizzards)
        blizzard?   (set (map :pos blizzards))
        in-bounds?  (fn [[r c]] (and (<= 0 r (dec rows))
                                     (<= 0 c (dec cols))))
        obstructed? #(or (not (in-bounds? %)) (wall? %) (blizzard? %))]
    (assoc state
           :blizzards blizzards
           :obstructed? obstructed?)))

(defn open-positions [state pos]
  (sequence
   (comp
    (map #(v+ pos %))
    (remove (:obstructed? state)))
   cardinals))

(defn shortest-path [states s0 goal-pos]
  (loop [time (:time s0), branches #{(:pos s0)}]
    (if (branches goal-pos)
      time
      (let [time'     (inc time)
            state'    (nth states time')
            branches' (mapcat #(open-positions state' %) branches)]
        (recur time' (set branches'))))))

(defn p1 [states goal-pos]
  (let [start-pos [0 1]]
    (shortest-path states {:time 0 :pos start-pos} goal-pos)))

(defn p2 [states goal-pos]
  (let [start-pos [0 1]
        t1        (shortest-path states {:time 0 :pos start-pos} goal-pos)
        t2        (shortest-path states {:time t1 :pos goal-pos} start-pos)
        t3        (shortest-path states {:time t2 :pos start-pos} goal-pos)]
    t3))

(comment
  (let [input    (->> (slurp "data/input-24.txt") parse)
        states   (iterate tick input)
        goal-pos [(- (:rows input) 1) (- (:cols input) 2)]]
    [(p1 states goal-pos)
     (p2 states goal-pos)]))
