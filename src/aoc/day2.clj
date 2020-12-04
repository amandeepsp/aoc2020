(ns aoc.day2
  (:require [aoc.shared :refer [file-lines]]))

(defn parse-line [s]
  (let [[_ min max char string] (re-find #"(\d+)-(\d+) (.): (.*)" s)]
    [(Integer/parseInt min) (Integer/parseInt max) (first char) string]))

(defn parse-input
  [file-name]
  (->> (file-lines file-name)
       (map parse-line)))

(defn is-valid?
  [[min max char string]]
  (let [freq-map (frequencies string)]
    (<= min (get freq-map char) max)))

(defn count-valid
  [file-name fn]
  (let [db (parse-input file-name)]
    (count (filter fn db))))

(defn part-1
  [file-name]
  (count-valid file-name is-valid?))

(defn is-valid-part2?
  [[min max char string]]
  (not=
    (= (nth string (dec min)) char)
    (= (nth string (dec max)) char)))

(defn part-2
  [file-name]
  (count-valid file-name is-valid-part2?))
