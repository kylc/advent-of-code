(ns aoc-2022.day10
  (:require
   [clojure.string :as str]))

(def cycles-of-interest #{20 60 100 140 180 220})

(defn parse [line]
  (let [[op v] (str/split line #" ")]
    (case op
      "noop" [{:op op}]
      "addx" [{:op "noop"}
              {:op op :v (parse-long v)}])))

(defn run [{:keys [cycle x]} {:keys [op v]}]
  {:cycle (inc cycle)
   :x (case op
        "noop" x
        "addx" (+ x v))})

(defn draw [crt {:keys [cycle x]}]
  (let [r       (quot cycle 40)
        c       (rem cycle 40)
        sprite? (< (abs (- x c)) 2)]
    (assoc-in crt [r c] sprite?)))

(defn signal-strengh [{:keys [cycle x]}]
  (* cycle x))

(defn p1 [ops]
  (let [s0 {:cycle 0 :x 1}]
    (->> ops
         (reductions run s0)
         (map signal-strengh)
         (keep-indexed #(when (cycles-of-interest %1) %2))
         (apply +))))

(defn p2 [ops]
  (let [s0   {:cycle 0 :x 1}
        crt0 (vec (repeat 6 (vec (repeat 40 false))))]
    (->> ops
         (reductions run s0)
         (reduce draw crt0)
         (map #(map (fn [x] (if x "#" ".")) %))
         (map str/join)
         (str/join "\n"))))

(comment
  (let [ops (->> (slurp "data/input-10.txt")
                 (str/split-lines)
                 (mapcat parse))]
    [(p1 ops)
     (p2 ops)]))
