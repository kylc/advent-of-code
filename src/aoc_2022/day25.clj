(ns aoc-2022.day25
  (:require
   [clojure.string :as str]
   [clojure.set :refer [map-invert]]))

(def d->s
  {-2 \=
   -1 \-
   0  \0
   1  \1
   2  \2})

(def s->d (map-invert d->s))

(defn powi [a b]
  ;; WARNING: for small-ish numbers only
  (long (Math/pow a b)))

(defn to-dec [s]
  (->> (reverse s)
       (map-indexed (fn [i ch] (* (powi 5 i) (s->d ch))))
       (reduce +)))

(defn to-snafu [n]
  (loop [n n, s '()]
    (if (zero? n)
      (apply str s)
      (let [q (quot n 5)
            m (mod n 5)]
        (if (>= m 3)
          (recur (inc q) (conj s (d->s (- m 5))))
          (recur q (conj s (d->s m))))))))

(comment
  (let [input (->> (slurp "data/input-25.txt") (str/split-lines))]
    (to-snafu (reduce + (map to-dec input)))))
