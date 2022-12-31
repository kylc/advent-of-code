(ns aoc-2022.day2
  (:require
   [clojure.string :as str]
   [clojure.set :refer [map-invert]]))

(def codex
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def choice-score
  {:rock     1
   :paper    2
   :scissors 3})

(def beats
  {:rock     :paper
   :paper    :scissors
   :scissors :rock})

(def beat-by (map-invert beats))

(defn decode-with [cdx s]
  (->> (str/split s #"\s")
       (mapv cdx)))

(defn play-score [a b]
  (cond
    (= a b)         3
    (= (beats a) b) 6
    (= (beats b) a) 0))

(defn score [a b]
  (+ (choice-score b) (play-score a b)))

(def codex2
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :lose
   "Y" :draw
   "Z" :win})

(defn score2 [a k]
  (let [b (case k
            :lose (beat-by a)
            :draw a
            :win  (beats a))]
    (score a b)))

(comment
  (let [input (->> (slurp "data/input-2.txt") (str/split-lines))]
    [(->> input
          (map #(decode-with codex %))
          (map #(apply score %))
          (apply +))
     (->> input
          (map #(decode-with codex2 %))
          (map #(apply score2 %))
          (apply +))]))
