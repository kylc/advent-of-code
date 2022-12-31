(ns aoc-2022.day6)

(defn find-packet [n s]
  (->> (partition n 1 s)
       (map #(count (set %)))
       (keep-indexed #(when (= %2 n) %1))
       (first)
       (+ n)))

(comment
  (let [input (slurp "data/input-6.txt")]
    [(find-packet 4 input)
     (find-packet 14 input)]))
