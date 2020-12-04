(ns aoc.day1
  (:require [aoc.shared :refer [file-lines]]))

(defn part-1
  [file-name]
  (let [sum-map (->> (file-lines file-name)
                     (map #(Integer/parseInt %))
                     (reduce #(assoc %1 %2 (- 2020 %2)) {}))]
    (->> (filter (fn [[key val]]
                   (and (= 2020 (+ key val)) (contains? sum-map val))) sum-map)
         (map (fn [[key val]] (* key val))))))

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(defn part-2
  [file-name]
  (let
    [numbers (->> (file-lines file-name)
                  (map #(Integer/parseInt %)))
     sum-map (reduce #(assoc %1 %2 (- 2020 %2)) {} numbers)
     pairs (all-pairs numbers)]

    (->> (filter (fn [[first second]]
                   (contains? sum-map (- 2020 (+ first second)))) pairs)
         (map (fn [[first second]]
                (* first second (- 2020 (+ first second))))))))
