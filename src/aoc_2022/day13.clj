(ns aoc-2022.day13
  (:require
   [clojure.string :as str]))

(defn parse [input]
  (->> input
       (str/split-lines)
       (filter not-empty)
       (map read-string)))

(defn in-order? [l r]
  (cond
    (and (number? l) (number? r))
    (cond
      (< l r) true
      (> l r) false)

    (and (coll? l) (coll? r))
    (let [result (->> (map in-order? l r) (filter some?) first)]
      (cond
        (some? result)          result
        (< (count l) (count r)) true
        (> (count l) (count r)) false))

    (coll? l)
    (in-order? l [r])

    (coll? r)
    (in-order? [l] r)))

(defn p1 [input]
  (let [xf (comp
            (partition-all 2)
            (keep-indexed (fn [idx [l r]]
                            (when (in-order? l r)
                              (inc idx)))))]
    (transduce xf + input)))

(defn p2 [input]
  (let [d1     [[2]]
        d2     [[6]]
        input  (conj input d1 d2)
        sorted (sort in-order? input)]
    (* (inc (.indexOf sorted d1))
       (inc (.indexOf sorted d2)))))

(comment
  (let [input (parse (slurp "data/input-13.txt"))]
    [(p1 input)
     (p2 input)]))
