(ns aoc.day3
  (:require [aoc.shared :refer [file-lines]]))

(defn parse-input [file-name]
  (map seq (file-lines file-name)))

(defn trees-seq [coll n]
  (let [reps (repeat n coll)]
    (reduce (fn [acc item]
              (map #(concat %1 %2) acc item))
            reps)))

(defn walk-pattern
  ([pattern dx dy]
   (walk-pattern pattern dx dy 0 0))
  ([pattern dx dy x y]
   (let [height (count pattern)
         width (count (first pattern))]
     (if (or (>= x width)
             (>= y height)) nil
                            (cons
                              (nth (nth pattern y) x)
                              (walk-pattern pattern dx dy (+ x dx) (+ y dy)))))))

(defn is-tree? [char] (= char \#))

(defn count-trees [file-name dx dy]
  (let [single-pattern (parse-input file-name)
        width (count (first single-pattern))
        height (count single-pattern)
        total-right (* (/ dx dy) height)
        rep-counts (/ total-right width)
        pattern (trees-seq single-pattern (int (Math/ceil rep-counts)))]
    (count (filter is-tree? (walk-pattern pattern dx dy)))))

(defn part-1 [file-name]
  (count-trees file-name 3 1))

(defn part-2 [file-name]
  (let [slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (reduce * (map (fn [[dx dy]] (count-trees file-name dx dy))
                   slopes))))