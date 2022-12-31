(ns aoc-2022.day5
  (:require
   [clojure.string :as str]))

(defn parse-stack-line [stacks line]
  (let [items (map #(nth % 1) (partition-all 4 line))
        item? #(Character/isUpperCase %)]
    (reduce-kv (fn [m k v] (update m k conj v))
               stacks
               (->> items
                    (keep-indexed (fn [i v] (when (item? v) [(inc i) v])))
                    (into {})))))

(defn parse-stacks [lines]
  (loop [stacks {}, lines lines]
    (if-let [line (first lines)]
      (recur (parse-stack-line stacks line) (rest lines))
      (->> (map (fn [[k v]] [k (vec v)]) stacks)
           (into (sorted-map))))))

(defn parse-move [s]
  (let [[[_ n from to] & _] (re-seq #"move (\d+) from (\d+) to (\d+)" s)]
    {:n    (parse-long n)
     :from (parse-long from)
     :to   (parse-long to)}))

(defn execute [stacks {:keys [n from to] :as move}]
  (if (zero? n)
    stacks
    (let [x (->> (stacks from) (peek))]
      (-> stacks
          (update from pop)
          (update to conj x)
          (execute (update move :n dec))))))

(defn execute2 [stacks {:keys [n from to]}]
  (let [xs (->> (stacks from) (take-last n))]
    (-> stacks
        (update from #(vec (drop-last n %)))
        (update to #(apply conj % xs)))))

(defn run [stacks moves exec-fn]
  (let [stacks (loop [stacks stacks, moves moves]
                 (if-let [move (first moves)]
                   (recur (exec-fn stacks move) (rest moves))
                   stacks))]
    (apply str (map peek (vals stacks)))))

(defn p1 [stacks moves]
  (run stacks moves execute))

(defn p2 [stacks moves]
  (run stacks moves execute2))

(comment
  (let [input          (slurp "data/input-5.txt")
        [stacks moves] (str/split input #"\n\n")
        stacks         (parse-stacks (str/split-lines stacks))
        moves          (mapv parse-move (str/split-lines moves))]
    [(p1 stacks moves)
     (p2 stacks moves)]))
