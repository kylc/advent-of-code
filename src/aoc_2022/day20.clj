(ns aoc-2022.day20
  (:require
   [clojure.string :as str]))

(def decryption-key 811589153)

(defn parse [input]
  (->> input
       (str/split-lines)
       (mapv read-string)))

(defn update-index [idxs i j]
  (let [pos      (some (fn [[k v]] (when (= v i) k)) idxs)
        shuffled (->> idxs
                      (map (fn [[k v]]
                             (cond
                               (<= i v j) [k (dec v)]
                               (<= j v i) [k (inc v)]
                               :else      [k v])))
                      (into {}))]
    (assoc shuffled pos j)))

(defn mix [input n]
  (let [idxs0 (->> input (map-indexed (fn [i _] [i i])) (into {}))
        inner (fn [idxs]
                (loop [i 0, idxs idxs]
                  (if (= i (count input))
                    idxs
                    (let [v   (nth input i)
                          src (idxs i)
                          dst (mod (+ src v) (dec (count input)))]
                      (recur (inc i) (update-index idxs src dst))))))]
    (as-> idxs0 $
      (iterate #(inner %) $)
      (nth $ n)
      (reduce (fn [coll [orig new]] (assoc coll new (nth input orig)))
              (vec (repeat (count input) 0))
              $))))

(defn p1 [input]
  (let [mixed (mix input 1)
        off   (.indexOf mixed 0)]
    (->> (map #(nth mixed (mod (+ off %) (count mixed))) [1000 2000 3000])
         (reduce +))))

(defn p2 [input]
  (let [input (map #(* % decryption-key) input)
        mixed (mix input 10)
        off   (.indexOf mixed 0)]
    (->> (map #(nth mixed (mod (+ off %) (count mixed))) [1000 2000 3000])
         (reduce +))))

(comment
  (let [input (parse (slurp "data/input-20.txt"))]
    [(p1 input)
     (p2 input)]))
