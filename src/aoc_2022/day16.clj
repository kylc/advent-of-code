(ns aoc-2022.day16
  (:require
   [clojure.string :as str]
   [clojure.data.priority-map :refer [priority-map]]))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals #(+ % d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn parse [lines]
  (->>
   (for [line (str/split-lines lines)]
     (let [[valve & tunnels] (re-seq #"[A-Z]{2}" line)
           flow              (first (re-seq #"\d+" line))]
       {valve {:flow (parse-long flow) :tunnels tunnels}}))
   (apply merge)))

(defn gen-dist-map [connectivity]
  (->>
   (for [source (keys connectivity)]
     [source (dijkstra source #(->> (get connectivity %) (map (fn [x] [x 1])) (into {})))])
   (into {})))

(defn neighbors [flow-rates dists {:keys [location open time-left flow-rate flow]}]
  (when (pos? time-left)
    (let [neighbors (filter #(and (not (open %)) (pos? (flow-rates %))) (keys dists))]
      (->>
       (conj
        (for [neighbor neighbors
              :let     [dist (+ 1 (dists neighbor))
                        time-after (- time-left dist)]
              :when    (pos? time-after)]
          {:location  neighbor
           :open      (conj open neighbor)
           :time-left time-after
           :flow      (+ flow (* dist flow-rate))
           :flow-rate (+ flow-rate (flow-rates neighbor))})
        {:location  location
         :open      open
         :time-left 0
         :flow      (+ flow (* time-left flow-rate))
         :flow-rate flow-rate})))))

(defn flow-paths [f t]
  (let [start {:location "AA" :open #{} :time-left t :flow-rate 0 :flow 0}]
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY start), r #{}]
      (if-let [path (peek q)]
        (recur (apply conj (pop q) (f path)) (if (zero? (:time-left path))
                                                   (conj r path)
                                                   r))
        r))))

(defn p1 [flow-rates dists]
  (let [f (fn [path]
            (let [dist-map (dists (:location path))]
                 (neighbors flow-rates dist-map path)))]
    (->> (flow-paths f 30)
         (map :flow)
         (apply max))))

(defn p2 [flow-rates dists]
  (let [f            (fn [path]
                       (let [dist-map (dists (:location path))]
                         (neighbors flow-rates dist-map path)))
        paths        (flow-paths f 26)
        best-by-open (reduce (fn [best path]
                               (update best (:open path) (fnil max 0) (:flow path)))
                             {}
                             paths)]
    (->> (for [[o1 flow1] best-by-open
               [o2 flow2] best-by-open
               :when      (not-any? o1 o2)]
           (+ flow1 flow2))
         (apply max))))

(comment
  (let [input      (->> (slurp "data/input-16.txt") parse)
        flow-rates (into {} (map (fn [[k v]] [k (:flow v)]) input))
        conns      (into {} (map (fn [[k v]] [k (set (:tunnels v))]) input))
        dists      (gen-dist-map conns)]
    [(p1 flow-rates dists)
     (p2 flow-rates dists)]))
