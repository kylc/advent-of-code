(ns aoc-2022.day1
  (:require
   [clojure.string :as str]
   [clojure.walk :refer [postwalk]]))

(defn parse-calories [str]
  (->>
   (str/split str #"\n\n")
   (map str/split-lines)
   (postwalk #(if (string? %)
                (Integer/parseInt %)
                %))
   (map #(reduce + %))))

(defn top-n [n cals]
  (->> cals
       (sort >)
       (take n)
       (reduce +)))

(comment
  (let [input (slurp "data/input-1.txt")]
    [(top-n 1 (parse-calories input))
     (top-n 3 (parse-calories input))]))
