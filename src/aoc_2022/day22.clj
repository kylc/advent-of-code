(ns aoc-2022.day22
  (:require
   [clojure.string :as str]))

(def dir->vec
  {"^" [-1 0]
   ">" [0 1]
   "v" [1 0]
   "<" [0 -1]})

(def dir->pass
  {">" 0
   "v" 1
   "<" 2
   "^" 3})

(defn rotate [facing cmd]
  (case [facing cmd]
    [">" "R"] "v"
    ["v" "R"] "<"
    ["<" "R"] "^"
    ["^" "R"] ">"

    [">" "L"] "^"
    ["^" "L"] "<"
    ["<" "L"] "v"
    ["v" "L"] ">"

    facing))

(defn v+ [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn parse [input]
  (let [[board path] (str/split input #"\n\n")
        lines        (str/split-lines board)]
    {:board (->> (for [row  (range (count lines))
                       col  (range (count (get lines row)))
                       :let [value (get-in lines [row col])]]
                   [[row col] (str value)])
                 (into {}))
     :path  (for [[_ n dir] (re-seq #"(\d+)(R|L)*" path)]
              [(parse-long n) dir])}))

(defn wrap1 [board pos facing]
  (let [next-pos  (v+ pos (dir->vec facing))
        [row col] next-pos]
    (if (not= (get board next-pos " ") " ")
      next-pos
      (case facing
        ">" (->> board
                    (keep (fn [[[r c] _]] (when (and (= r row)
                                                     (not= " " (get board [r c] " ")))
                                            [r c])))
                    (apply min-key (fn [[_ c]] c)))
        "v" (->> board
                    (keep (fn [[[r c] _]] (when (and (= c col)
                                                     (not= " " (get board [r c] " ")))
                                            [r c])))
                    (apply min-key (fn [[r _]] r)))
        "<" (->> board
                    (keep (fn [[[r c] _]] (when (and (= r row)
                                                     (not= " " (get board [r c] " ")))
                                            [r c])))
                    (apply max-key (fn [[_ c]] c)))
        "^" (->> board
                    (keep (fn [[[r c] _]] (when (and (= c col)
                                                     (not= " " (get board [r c] " ")))
                                            [r c])))
                    (apply max-key (fn [[r _]] r)))))))

(defn move1 [board [n dir] pos facing]
  (loop [pos pos, n n]
    (if (zero? n)
      [pos (rotate facing dir)]
      (let [next-pos (wrap1 board pos facing)
            obstacle (get board next-pos " ")]
        (case obstacle
          "." (recur next-pos (dec n))
          "#" (recur pos 0))))))

(defn top-left [n region]
  (case region
    1 [0 (* 1 n)]
    2 [0 (* 2 n)]
    3 [(* 1 n) (* 1 n)]
    4 [(* 2 n) (* 1 n)]
    5 [(* 2 n) 0]
    6 [(* 3 n) 0]))

(defn region [n [row col]]
  (->> (for [region (range 1 7)
             :let [[r c] (top-left n region)]
             :when (and (<= r row (+ r (dec n)))
                        (<= c col (+ c (dec n))))]
         region)
       first))

(defn wrap2 [board [r c :as pos] facing]
  (let [n        50
        next-pos (v+ pos (dir->vec facing))
        [rr rc]  [(mod r n) (mod c n)]]
    (if (not= (get board next-pos " ") " ")
      [next-pos facing]
      (let [[reg' [rr rc] facing']
            (case [(region n pos) facing]
              [1 "<"] [5 [(- (dec n) rr) 0] ">"]
              [1 "^"] [6 [rc 0] ">"]

              [2 "^"] [6 [(dec n) rc] "^"]
              [2 ">"] [4 [(- (dec n) rr) (dec n)] "<"]
              [2 "v"] [3 [rc (dec n)] "<"]

              [3 "<"] [5 [0 rr] "v"]
              [3 ">"] [2 [(dec n) rr] "^"]

              [4 ">"] [2 [(- (dec n) rr) (dec n)] "<"]
              [4 "v"] [6 [rc (dec n)] "<"]

              [5 "^"] [3 [rc 0] ">"]
              [5 "<"] [1 [(- (dec n) rr) 0] ">"]

              [6 ">"] [4 [(dec n) rr] "^"]
              [6 "v"] [2 [0 rc] "v"]
              [6 "<"] [1 [0 rr] "v"])]
        [(v+ (top-left n reg') [rr rc]) facing']))))

(defn move2 [board [n dir] pos facing]
  (loop [pos pos, facing facing, n n]
    (if (zero? n)
      [pos (rotate facing dir)]
      (let [[next-pos next-facing] (wrap2 board pos facing)
            obstacle               (get board next-pos " ")]
        (case obstacle
          "." (recur next-pos next-facing (dec n))
          "#" (recur pos facing 0))))))

(defn solve [move-fn board path]
  (loop [path path, [r c :as pos] [0 50], facing ">"]
    (if-let [p (first path)]
      (let [[pos facing] (move-fn board p pos facing)]
        (recur (rest path) pos facing))
      (let [f (dir->pass facing)]
        (+ (* 1000 (inc r)) (* 4 (inc c)) f)))))

(defn p1 [board path]
  (solve move1 board path))

(defn p2 [board path]
  (solve move2 board path))

(comment
  (let [input                (slurp "data/input-22.txt")
        {:keys [board path]} (parse input)]
    [(p1 board path)
     (p2 board path)]))
