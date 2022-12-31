(ns aoc-2022.day12
  (:require
   [clojure.string :as str]
   [clojure.data.priority-map :refer [priority-map]]))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn index-of-2d [map v]
  (->>
   (for [i     (range 0 (count map))
         j     (range 0 (count (nth map 0)))
         :when (= v (get-in map [i j]))]
     [i j])
   (first)))

(defn to-height [v]
  (let [v (case v
            \S \a
            \E \z
            v)]
    (- (int v)
       (int \a))))

(defn parse [input]
  (let [raw   (->> (str/split-lines input)
                   (mapv #(vec %)))
        start (index-of-2d raw \S)
        end   (index-of-2d raw \E)]
    {:map   (mapv #(mapv to-height %) raw)
     :start start
     :end   end}))

(defn traversable? [h1 h2]
  (and h1 h2 (<= (- h2 h1) 1)))

(defn neighbors [map pos]
  (let [[x y] pos
        h     (get-in map pos)
        up    [x (dec y)]
        right [(inc x) y]
        down  [x (inc y)]
        left  [(dec x) y]]
    (->>
     (for [v     [up right down left]
           :when (traversable? h (get-in map v))]
       v)
     (mapv (fn [x] {x 1}))
     (into {}))))

(defn p1 [{:keys [start end map]}]
  (-> (dijkstra start #(neighbors map %))
      (get end)))

(defn p2 [{:keys [end map]}]
  (let [starts (for [i     (range (count map))
                     j     (range (count (nth map 0)))
                     :when (= 0 (get-in map [i j]))]
                 [i j])]
    (->> (for [start starts
               :let  [dist (-> (dijkstra start #(neighbors map %))
                               (get end))]
               :when dist]
           dist)
         (apply min))))

(comment
  (let [input (->> (slurp "data/input-12.txt") parse)]
    [(p1 input)
     (p2 input)]))
