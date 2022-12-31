(ns aoc-2022.day3
  (:require
   [clojure.string :as str]
   [clojure.set :refer [intersection]]))

(defn priority [x]
  (let [i (int x)]
    (if (<= (int \a) i (int \z))
      (+ i (- (int \a)) 1)
      (+ i (- (int \A)) 27))))

(defn compartments [coll]
  (let [n (/ (count coll) 2)]
    (split-at n coll)))

(defn intersect [colls]
  (->> colls
       (map set)
       (reduce intersection)
       (first)))

(defn p1 [lines]
  (let [xf (map #(priority (intersect (compartments %))))]
    (transduce xf + lines)))

(defn p2 [lines]
  (let [xf (comp
            (partition-all 3)
            (map #(priority (intersect %))))]
    (transduce xf + lines)))

(comment
  (let [input (->> (slurp "data/input-3.txt")
                   (str/split-lines)
                   (map vec))]
    [(p1 input)
     (p2 input)]))
