(ns aoc-2022.day21
  (:require
   [clojure.string :as str]))

(defn parse [input replace-root?]
  (->>
   (for [line (str/split-lines input)]
     (let [[name expr] (mapv str/trim (str/split line #":"))
           [a op b]    (str/split expr #" ")
           op          (if (and replace-root? (= "root" name)) "=" op)]
       (if-let [v (parse-long expr)]
         [name v]
         [name [a op b]])))
   (into {})))

(defn run [input]
  (->> (for [[monkey expr] input]
         (if (number? expr)
           ;; already solved
           [monkey expr]
           (let [[pa op pb] expr
                 a          (get input pa)
                 b          (get input pb)]
             (if (and (number? a) (number? b))
               ;; able to solve now
               [monkey (case op
                         "=" (- a b)
                         "+" (+ a b)
                         "-" (- a b)
                         "*" (* a b)
                         "/" (/ a b))]
               ;; can't solve yet
               [monkey expr]))))
       (into {})))

(defn run-to-completion [input]
  (->> (iterate run input)
       (keep #(let [root (get % "root")]
                (when (number? root)
                  root)))
       (first)))

(defn binary-search [lo hi f]
  (loop [lo lo, hi hi]
    (when (<= lo hi)
      (let [pivot (Math/floorDiv (+ lo hi) 2)
            error (f pivot)]
        (cond
          (zero? error) pivot
          (neg? error)  (recur (inc pivot) hi)
          (pos? error)  (recur lo (dec pivot)))))))

(defn p1 [input]
  (run-to-completion input))

(defn p2 [input]
  (let [lo  0
        hi  (bit-shift-left 1 50)
        f   (fn [v] (-> input (assoc "humn" v) run-to-completion))
        dir (if (<= (f 0) (f 100)) 1 -1)]
    (binary-search lo hi #(* dir (f %)))))

(comment
  (let [input (slurp "data/input-21.txt")]
    [(p1 (parse input false))
     (p2 (parse input true))]))
