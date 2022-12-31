(ns aoc-2022.day14
  (:require
   [clojure.string :as str]))

(defn parse [input]
  (for [line (str/split-lines input)]
    (for [[_ x y] (re-seq #"(\d+),(\d+)" line)]
        [(parse-long x) (parse-long y)])))

(defn line [[x1 y1] [x2 y2]]
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
    [x y]))

(defn build-map [lines]
  (->>
   (for [l  lines
         ps (partition 2 1 l)]
     (apply line ps))
   (apply concat)
   (into #{})))

(defn solve
  [m source abyss floor]
  (loop [[x y] source]
    (let [d  [(+ x 0) (+ y 1)]
          dl [(- x 1) (+ y 1)]
          dr [(+ x 1) (+ y 1)]]
         (cond
           (> y abyss)       nil
           (= (+ 1 y) floor) [x y]
           (not (m d))       (recur d)
           (not (m dl))      (recur dl)
           (not (m dr))      (recur dr)
           ;; nowhere to go!
           :else             [x y]))))

(defn max-y [m]
  (apply max (map second m)))

(defn p1 [m]
  (let [abyss (max-y m)]
    (loop [m m, c 0]
      (if-let [s (solve m [500 0] abyss ##Inf)]
        (recur (conj m s) (inc c))
        c))))

(defn p2 [m]
  (let [floor     (+ 2 (max-y m))
        complete? #(= [500 0] %)]
    (loop [m m, c 0]
      (let [s (solve m [500 0] ##Inf floor)]
        (if (complete? s)
          (inc c)
          (recur (conj m s) (inc c)))))))

(comment
  (let [m (->> (slurp "data/input-14.txt") parse build-map)]
    [(p1 m)
     (p2 m)]))
