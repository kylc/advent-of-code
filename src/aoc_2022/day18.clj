(ns aoc-2022.day18
  (:require
   [clojure.string :as str]))

(defn bfs [start f]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY start) r #{start}]
    (if-let [v (peek q)]
      (let [ns (->> (f v) (remove r))]
        (recur (into (pop q) ns) (into r ns)))
      r)))

(defn parse [input]
  (->> (for [line (str/split-lines input)]
         (map parse-long (re-seq #"\d+" line)))
       (set)))

(defn sides [[x y z]]
  (for [[dx dy dz] [[-1 0 0] [1 0 0]
                    [0 -1 0] [0 1 0]
                    [0 0 -1] [0 0 1]]]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn p1 [points]
  (->> (mapcat sides points)
       (remove points)
       (count)))

(defn p2 [points]
  (let [bbox     (fn [[x y z]] (and (<= -5 x 25) (<= -5 y 25) (<= -5 z 25)))
        open-air (bfs [-5 -5 -5] #(->> (sides %) (remove points) (filter bbox)))]
    (->> (mapcat sides points)
         (remove points)
         (filter open-air)
         (count))))

(comment
  (let [points (parse (slurp "data/input-18.txt"))]
    [(p1 points)
     (p2 points)]))
