(ns aoc-2022.day19
  (:require
   [clojure.string :as str]))

(def material-types [:ore :clay :obsidian :geode])

(defn parse [input]
  (for [blueprint (str/split-lines input)]
    (let [[_ o c obo obc go gob] (re-seq #"\d+" blueprint)]
      {:ore      {:ore (read-string o)}
       :clay     {:ore (read-string c)}
       :obsidian {:ore (read-string obo) :clay (read-string obc)}
       :geode    {:ore (read-string go) :obsidian (read-string gob)}})))

(defn max-cost [blueprint mat]
  (apply max (map #(get % mat 0) (vals blueprint))))

(defn tick [{:keys [robots] :as state}]
  (-> state
      (update :materials #(merge-with + % robots))
      (update :time-left dec)))

(defn can-build? [blueprint {:keys [materials]} mat]
  (let [reqs (blueprint mat)]
    (every? (fn [[mat req]] (>= (materials mat) req)) reqs)))

(defn build [blueprint {:keys [robots materials] :as state} mat]
  (let [reqs (blueprint mat)]
    (assoc state
           :materials (merge-with - materials reqs)
           :robots (update robots mat inc))))

(defn iterate-build [blueprint state mat]
  (loop [state state]
    (when (> (:time-left state) 0)
      (if (can-build? blueprint state mat)
        (build blueprint (tick state) mat)
        (recur (tick state))))))

(defn explore [blueprint max-costs {:keys [robots] :as state}]
  (->>
   (for [mat   material-types
         ;; OPTIMIZATION: Always build a geode robot when possible, but don't
         ;; bother building more robots of other types than needed to keep up
         ;; with maximum demand.
         :when (or (= :geode mat) (< (robots mat) (max-costs mat)))]
     (iterate-build blueprint state mat))
   (remove nil?)))

(defn score [state]
  (-> state :materials :geode))

(defn blueprint-score [blueprint time]
  (let [state     {:materials {:ore 0 :clay 0 :obsidian 0 :geode 0}
                   :robots    {:ore 1 :clay 0 :obsidian 0 :geode 0}
                   :time-left time}
        max-costs (into {} (map (fn [m] [m (max-cost blueprint m)]) [:ore :clay :obsidian :geode]))
        score-map (fn [s] {(:time-left s) (score s)})]
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY state) r #{} best {}]
      (if-let [v (peek q)]
        (let [ns (->> (explore blueprint max-costs v)
                      (remove r)
                      (filter #(>= (score %) (get best (:time-left %) 0))))]
          (recur (into (pop q) ns)
                 (conj r v)
                 (apply merge-with max best (map score-map ns))))
        (best 0)))))

(defn p1 [blueprints]
  (let [blueprints (map-indexed (fn [idx b] [(inc idx) b]) blueprints)
        scores     (pmap (fn [[id blueprint]]
                           (* id (blueprint-score blueprint 24)))
                         blueprints)]
    (reduce + scores)))

(defn p2 [blueprints]
  (let [blueprints (take 3 blueprints)
        scores     (pmap #(blueprint-score % 32) blueprints)]
    (reduce * scores)))

(comment
  (let [blueprints (parse (slurp "data/input-19.txt"))]
    [(p1 blueprints)
     (p2 blueprints)]))
