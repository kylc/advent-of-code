(ns aoc-2022.day7
  (:require
   [clojure.string :as str]))

(defn parse [state line]
  (let [{:keys [cwd tree]} state
        words (str/split line #" ")]
    (->>
     (condp #(str/starts-with? %2 %1) line
       "$ ls"
       {}

       "$ cd"
       (let [dir (last words)]
         {:cwd (if (= dir "..")
                 (pop cwd)
                 (conj cwd dir))})

       "dir"
       (let [[_ dirname] words]
         {:tree (assoc-in tree (conj cwd dirname) {})})

       ;; file w/ size
       (let [[size filename] words]
         {:tree (assoc-in tree (conj cwd filename) (read-string size))}))
     (apply merge state))))

(defn tree-size [m]
  (->> m
       (vals)
       (map #(if (map? %) (tree-size %) %))
       (apply +)))

(defn nested-sizes [m]
  (->> m
       (tree-seq map? vals)
       (filter map?)
       (map tree-size)))

(defn p1 [tree]
  (let [max-size 100000]
    (->> (nested-sizes tree)
         (filter #(< % max-size))
         (apply +))))

(defn p2 [tree]
  (let [need-disk (- 70000000 30000000)
        used-disk (tree-size tree)
        to-free (- used-disk need-disk)]
    (->> (nested-sizes tree)
         (filter #(> % to-free))
         (sort)
         (first))))

(comment
  (let [input (str/split-lines (slurp "data/input-7.txt"))
        tree (:tree (reduce parse {:cwd [] :tree {}} input))]
    [(p1 tree)
     (p2 tree)]))
