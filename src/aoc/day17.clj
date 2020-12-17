(ns aoc.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def init-pattern
  (->> (io/resource "day17.txt")
       (slurp)
       (str/split-lines)
       (map #(map {\. false \# true} %))))

(defn get-cells [init-pattern]
  (for [[i row] (map-indexed list init-pattern)
        [j cell] (map-indexed list row)]
    (if (true? cell)
      [{:x i :y j :z 0} true]
      [{:x i :y j :z 0} false])))

(def init-state (into {} (get-cells init-pattern)))

(defn neighbors [{:keys [x y z]}]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not= 0 dx dy dz)]
    {:x (+ x dx) :y (+ y dy) :z (+ z dz)}))

(defn count-neighbors [cell cells]
  (count (filter true? (map #(get cells % false) (neighbors cell)))))

(defn apply-rule [cells cell]
  (let [is-active (get cells cell false)
        active-neighbors (count-neighbors cell cells)]
    (cond
      (and is-active (#{2 3} active-neighbors)) true
      (and (not is-active) (= 3 active-neighbors)) true
      :else false)))

(defn grow-map [cells]
  (let [grown-map (->> (keys cells)
                       (mapcat neighbors)
                       (map #(vector % false))
                       (into {}))]
    (merge grown-map cells)))

(defn next-state [cells]
  (let [grown-map (grow-map cells)]
    (reduce (fn [cells-state cell]
              (assoc cells-state cell (apply-rule grown-map cell)))
            {}
            (keys grown-map))))
;; Part 1
(->> (get-cells init-pattern)
     (into {})
     (iterate next-state)
     (take 7)
     (last)
     (filter #(true? (second %)))
     (count))

(defn get-cells* [init-pattern]
  (for [[i row] (map-indexed list init-pattern)
        [j cell] (map-indexed list row)]
    (if (true? cell)
      [{:x i :y j :z 0 :w 0} true]
      [{:x i :y j :z 0 :w 0} false])))

(defn neighbors* [{:keys [x y z w]}]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        dw [-1 0 1]
        :when (not= 0 dx dy dz dw)]
    {:x (+ x dx) :y (+ y dy) :z (+ z dz) :w (+ w dw)}))

;; Part 2
(with-redefs [neighbors neighbors*
              get-cells get-cells*]
  (->> (get-cells init-pattern)
       (into {})
       (iterate next-state)
       (take 7)
       (last)
       (filter #(true? (second %)))
       (count)))
