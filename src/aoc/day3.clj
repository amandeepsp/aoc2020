(ns aoc.day3
  (:require [aoc.shared :refer [file-lines]]))

(defn parse-input [file-name]
  (map seq (file-lines file-name)))


(defn walk-pattern
  ([pattern dx dy]
   (walk-pattern pattern dx dy 0 0))
  ([pattern dx dy x y]
   (let [height (count pattern)
         width (count (first pattern))]
     (if  (>= y height) nil
                            (cons
                              (nth (nth pattern y) x)
                              (walk-pattern pattern dx dy (mod (+ x dx) width) (+ y dy)))))))

(defn is-tree? [char] (= char \#))

(defn count-trees [file-name dx dy]
  (let [pattern (parse-input file-name)]
    (count (filter is-tree? (walk-pattern pattern dx dy)))))

(defn part-1 [file-name]
  (count-trees file-name 3 1))

(defn part-2 [file-name]
  (let [slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (reduce * (map (fn [[dx dy]] (count-trees file-name dx dy))
                   slopes))))
