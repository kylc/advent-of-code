(ns aoc-2022.day15
  (:require
   [clojure.string :as str]))

(defn dist [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn parse [input]
  (let [lines (str/split-lines input)]
    (for [line lines]
      (let [[sx sy bx by] (map parse-long (re-seq #"-?\d+" line))]
        {:sensor [sx sy]
         :beacon [bx by]
         :radius (dist [sx sy] [bx by])}))))

(defn isnt? [pairs xy]
  (some? (some #(>= (:radius %) (dist (:sensor %) xy))
         pairs)))

(defn perimeter [[x y] dist]
  (->> (for [i (range (inc dist))]
         (let [j (- dist i)]
           [[(- x i) (+ y j)]
            [(- x i) (- y j)]
            [(+ x i) (+ y j)]
            [(+ x i) (- y j)]]))
       (apply concat)))

(defn corners [[x y] d]
  [[(+ x d) y]
   [(- x d) y]
   [x (+ y d)]
   [x (- y d)]])

(defn binary-search [lo hi f]
  (loop [lo lo, hi hi]
    (when (<= lo hi)
      (let [pivot (Math/floorDiv (+ lo hi) 2)
            error (f pivot)]
        (cond
          (zero? error) pivot
          (neg? error)  (recur (inc pivot) hi)
          (pos? error)  (recur lo (dec pivot)))))))

(defn p1 [m]
  (let [y         2000000
        max-dist  (apply max (map #(dist (:sensor %) (:beacon %)) m))
        min-x     (apply min (map #(first (:sensor %)) m))
        max-x     (apply max (map #(first (:sensor %)) m))
        boundary? (fn [dir x]
                    (case [(isnt? m [(dec x) y]) (isnt? m [x y])]
                      [false false] (* dir 1)
                      [true true]   (* dir -1)
                      0))
        lb        (binary-search (- min-x max-dist) max-x #(boundary? -1 %))
        ub        (binary-search min-x (+ max-x max-dist) #(boundary? 1 %))]
    (- ub lb)))

(defn p2 [m max-coord]
  (let [[[x y]] (transduce (comp
                            (mapcat (fn [{:keys [sensor radius]}]
                                      (perimeter sensor (inc radius))))
                            (filter (fn [[x y]] (and (< 0 x max-coord) (< 0 y max-coord))))
                            (filter #(not (isnt? m %)))
                            (take 1))
                           conj
                           m)]
    (+ (* 4000000 x) y)))

(comment
  (let [m (->> (slurp "data/input-15.txt") parse)]
    [(p1 m)
     (p2 m 4000000)]))
