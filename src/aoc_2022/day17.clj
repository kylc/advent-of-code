(ns aoc-2022.day17
  (:require
   [clojure.string :as str]))

(def rocks
  [ ;; ####
   [[0 0] [1 0] [2 0] [3 0]]

   ;; .#.
   ;; ###
   ;; .#.
   [[1 2]
    [0 1] [1 1] [2 1]
    [1 0]]

   ;; ..#
   ;; ..#
   ;; ###
   [[2 2]
    [2 1]
    [0 0] [1 0] [2 0]]

   ;; #
   ;; #
   ;; #
   ;; #
   [[0 3]
    [0 2]
    [0 1]
    [0 0]]

   ;; ##
   ;; ##
   [[0 1] [1 1]
    [0 0] [1 0]]])

(defn move [floor [dx dy] rock]
  (let [new-rock (mapv (fn [[x y]] [(+ x dx) (+ y dy)]) rock)]
    ;; If any part of the moved rock is outside the L/R map boundary then
    ;; discard the move.
    (if (and (every? (fn [[x _]] (<= 0 x 6)) new-rock)
             (not-any? floor new-rock))
      new-rock
      rock)))

(defn spawn [floor rock-cycle highest]
  (move floor [2 (+ highest 4)] (first rock-cycle)))

(defn tick [{:keys [round floor rock-cycle jet-cycle max-height]}]
  (loop [rock      (spawn floor rock-cycle max-height)
         jet-cycle jet-cycle]
    (let [jet-vector  (case (first jet-cycle) \> [1 0] \< [-1 0])
          blown-rock  (move floor jet-vector rock)
          fallen-rock (move floor [0 -1] blown-rock)]
      (if (= blown-rock fallen-rock)
        {:round      (inc round)
         :floor      (apply conj floor fallen-rock)
         :rock-cycle (rest rock-cycle)
         :jet-cycle  (rest jet-cycle)
         :max-height (max max-height (apply max (map second fallen-rock)))}
        (recur fallen-rock (rest jet-cycle))))))

(defn partitionv [n xs]
  (for [i (range 0 (quot (count xs) n))]
    (subvec xs (* i n) (* (inc i) n))))

(defn detect-cycle [xs]
  (let [min-reps 5]
    (loop [n 2, off 0]
      (let [cycles (->> (subvec (subvec xs off) 0 (* min-reps n))
                        (partitionv n))]
        (if (apply = cycles)
          {:off off :window n :dscore (apply + (first cycles))}
          (cond
            (>= off (- (count xs) (* min-reps n) 1))
            (recur (inc n) 0)

            (>= n (/ (count xs) 2))
            nil

            :else
            (recur n (inc off))))))))

(defn p2 [heights]
  (let [dh (mapv #(apply - (reverse %)) (partition 2 1 heights))
        {:keys [off window dscore]} (detect-cycle dh)

        p (quot (- 1000000000000 off) window)
        q (rem (- 1000000000000 off) window)]
    (+ (apply + (subvec dh 0 off))
       (* dscore p)
       (apply + (subvec dh off (+ off q))))))

(comment
  (let [jets    (vec (str/trim-newline (slurp "data/input-17.txt")))
        rounds  (iterate tick {:round      0
                               :floor      (set (map (fn [x] [x 0]) (range 0 8)))
                               :rock-cycle (cycle rocks)
                               :jet-cycle  (cycle jets)
                               :max-height 0})
        heights (->> rounds
                     (map :max-height)
                     (take 20000)
                     (vec))]
    [(nth heights 2022)
     (p2 heights)]))
