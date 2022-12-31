(ns aoc-2022.day9
  (:require
   [clojure.string :as str]))

(defn parse-cmd [line]
  (let [[ch n] (str/split line #" ")]
    (repeat (parse-long n) (first ch))))

(defn follow [[hx hy] [tx ty]]
  (let [dx (- hx tx)
        dy (- hy ty)]
    (if (or (> (abs dx) 1)
            (> (abs dy) 1))
      [(+ tx (Long/signum dx))
       (+ ty (Long/signum dy))]
      [tx ty])))

(defn move [cmd [x y]]
  (case cmd
    \U [x (inc y)]
    \R [(inc x) y]
    \D [x (dec y)]
    \L [(dec x) y]))

(defn process [[head & tail] cmd]
  (let [head' (move cmd head)]
    (reduce
     (fn [r t] (conj r (follow (peek r) t)))
     [head']
     tail)))

(defn visit-count [n cmds]
  (let [rope (repeat (inc n) [0 0])]
    (->> cmds
         (reductions process rope)
         (map last)
         (set)
         (count))))

(comment
  (let [lines (str/split-lines (slurp "data/input-9.txt"))
        cmds  (mapcat parse-cmd lines)]
    [(visit-count 1 cmds)
     (visit-count 9 cmds)]))
