(ns aoc.day2
  (:require [clojure.string :as str]
            [aoc.shared :refer [file-lines]]))

(defn split-ranges
  [range]
  (str/split range #"-"))

(defn split-key [key]
  (let [space-split (str/split key #" ")
        ranges (map #(Integer/parseInt %) (split-ranges (first space-split)))
        chars (second space-split)]
    (cons ranges chars)))

(defn parse-input
  [file-name]
  (->> (file-lines file-name)
       (map #(str/split % #": "))
       (map (fn [[first & rest]]
              (cons (split-key first) rest)))))

(defn is-valid?
  [[[range, char], string]]
  (let [freq-map
        (into {} (for [[k v]
                       (group-by identity string)]
                   [k (count v)]))]
    (and (contains? freq-map char)
         (<= (first range) (get freq-map char))
         (>= (second range) (get freq-map char)))))

(defn count-valid
  [file-name fn]
  (let [db (parse-input file-name)]
    (count (filter fn db))))

(defn part-1
  [file-name]
  (count-valid file-name is-valid?))

(defn is-valid-part2?
  [[[range, char], string]]
  (let [first-index (- (first range) 1)
        second-index (- (second range) 1)]
    (or (and (= (nth string first-index) char)
             (not (= (nth string second-index) char)))
        (and (not (= (nth string first-index) char))
             (= (nth string second-index) char)))))

(defn part-2
  [file-name]
  (count-valid file-name is-valid-part2?))