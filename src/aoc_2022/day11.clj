(ns aoc-2022.day11
  (:require
   [clojure.string :as str]))

(defn parse-op [line]
  (let [[_ a op b] (first (re-seq #"(\d+|old) (\*|\+) (\d+|old)" line))
        [a op b]   [(case a
                      "old" :old
                      (parse-long a))
                    (case op
                      "*" *
                      "+" +)
                    (case b
                      "old" :old
                      (parse-long b))]]
    (fn [old]
      (op (if (= a :old) old a)
          (if (= b :old) old b)))))

(defn parse-monkey [lines]
  (let [[_ items op test if-true if-false] lines]
    {:items     (->> (re-seq #"\d+" items) (mapv parse-long))
     :divisor   (->> (re-seq #"\d+" test) first parse-long)
     :op        (parse-op op)
     :if-true   (->> (re-seq #"\d+" if-true) first parse-long)
     :if-false  (->> (re-seq #"\d+" if-false) first parse-long)
     :inspected 0}))

(defn tick [monkey worry-op]
  {:inspected (count (:items monkey))
   :throws    (for [worry (:items monkey)]
                (let [new-worry (worry-op ((:op monkey) worry))
                      dst       (if (zero? (rem new-worry (:divisor monkey)))
                                  (:if-true monkey)
                                  (:if-false monkey))]
                  {:dst dst :worry new-worry}))})

(defn process-throws [monkeys from throws]
  (reduce (fn [monkeys {:keys [dst worry]}]
            (let [remaining (-> (get-in monkeys [from :items]) pop)]
              (-> monkeys
                  (assoc-in [from :items] remaining)
                  (update-in [dst :items] conj worry))))
          monkeys
          throws))

(defn round [monkeys worry-op]
  (loop [idx 0, monkeys monkeys]
    (if-let [monkey (get monkeys idx)]
      (let [{:keys [throws inspected]} (tick monkey worry-op)]
        (recur
         (inc idx)
         (process-throws (update-in monkeys [idx :inspected] #(+ % inspected)) idx throws)))
      monkeys)))

(defn monkey-business [monkeys]
  (->> monkeys
       (mapv :inspected)
       (sort)
       (take-last 2)
       (apply *)))

(defn p1 [monkeys]
  (let [worry-op #(quot % 3)
        rounds   (iterate #(round % worry-op) monkeys)]
    (monkey-business (nth rounds 20))))

(defn p2 [monkeys]
  (let [lcm      (reduce * (map :divisor monkeys))
        worry-op #(mod % lcm)
        rounds   (iterate #(round % worry-op) monkeys)]
    (monkey-business (nth rounds 10000))))

(comment
  (let [input   (slurp "data/input-11.txt")
        monkeys (->> (str/split input #"\n\n")
                     (map str/split-lines)
                     (mapv parse-monkey))]
    [(p1 monkeys)
     (p2 monkeys)]))
