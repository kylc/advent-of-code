(ns aoc-2022.day23
  (:require
   [clojure.string :as str]))

(def directions
  {:n  [-1 0]
   :ne [-1 1]
   :e  [0 1]
   :se [1 1]
   :s  [1 0]
   :sw [1 -1]
   :w  [0 -1]
   :nw [-1 -1]})

(def default-cycle
  (cycle [[:n :ne :nw]
          [:s :se :sw]
          [:w :nw :sw]
          [:e :ne :se]]))

(defn v+ [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn parse [input]
  (let [grid (->> (str/split-lines input)
                  (mapv vec))
        w    (count (first grid))
        h    (count grid)]
    (for [r     (range h)
          c     (range w)
          :when (= \# (get-in grid [r c]))]
      [r c])))

(defn choose-move [elves elf cyc]
  (->> cyc
       (take 4)
       (keep (fn [dirs]
               (when (not-any? elves (map #(v+ elf (directions %)) dirs))
                 (directions (first dirs)))))
       (first)))

(defn plan [cyc elves]
  (for [elf elves]
    (let [chosen-move (choose-move elves elf cyc)]
      (cond
        (not-any? elves (map #(v+ elf %) (vals directions)))
        [elf [0 0]]

        (some? chosen-move)
        [elf chosen-move]

        :else
        [elf [0 0]]))))

(defn execute [elf-moves]
  (let [occ-count (->> (for [[e m] elf-moves]
                         {(v+ e m) 1})
                       (apply merge-with +))]
    (for [[pos move] elf-moves]
      (let [pos' (v+ pos move)]
        (if (= (occ-count pos') 1)
          pos'
          pos)))))

(defn bbox [pts]
  (let [x0 (apply min (map second pts))
        x1 (apply max (map second pts))
        y0 (apply min (map first pts))
        y1 (apply max (map first pts))]
    (* (inc (- x1 x0)) (inc (- y1 y0)))))

(defn p1 [elves]
  (let [end-state (loop [n 10, cyc default-cycle, elves elves]
                    (if (zero? n)
                      elves
                      (recur (dec n) (rest cyc) (->> (plan cyc elves)
                                                     (execute)
                                                     (set)))))]
    (- (bbox end-state) (count end-state))))

(defn p2 [elves]
  (loop [n 1, cyc default-cycle, elves elves]
    (let [new-elves (->> (plan cyc elves) execute set)]
      (if (= elves new-elves)
        n
        (recur (inc n) (rest cyc) new-elves)))))

(comment
  (let [elves (->> (slurp "data/input-23.txt") parse set)]
    [(p1 elves)
     (p2 elves)]))
