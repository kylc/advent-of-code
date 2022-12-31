(ns aoc-2022.day4
  (:require
   [clojure.string :as str]))

(defn parse-range [s]
  (let [[lb ub] (str/split s #"-")]
    [(parse-long lb) (parse-long ub)]))

(defn parse-line [s]
  (map parse-range (str/split s #",")))

(defn fully-contained? [a b]
  (or (<= (nth a 0) (nth b 0) (nth b 1) (nth a 1))
      (<= (nth b 0) (nth a 0) (nth a 1) (nth b 1))))

(defn overlap? [a b]
  (or (<= (nth a 0) (nth b 0) (nth a 1))
      (<= (nth b 0) (nth a 0) (nth b 1))))

(defn count-ranges [pred ranges]
  (->> (filter (fn [[a b]] (pred a b)) ranges)
       (count)))

(comment
  (let [ranges (->> (slurp "data/input-4.txt")
                    (str/split-lines)
                    (map parse-line))]
    [(count-ranges fully-contained? ranges)
     (count-ranges overlap? ranges)]))
