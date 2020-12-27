(ns aoc.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[_ part1 part2] (re-find #"(.*)\(contains (.*)\)" line)
        foods (str/split part1 #" ")
        allergens (str/split part2 #", ")]
    [allergens foods]))

(defn parse-input [file-name]
  (->> (io/resource file-name)
       slurp
       str/split-lines
       (map parse-line)))

(def allergens-with-foods (parse-input "day21.txt"))

(def allergens-map
  (reduce
    (fn [state [keys val]]
      (reduce
        (fn [istate key]
          (if (contains? istate key)
            (update istate key set/intersection (into #{} val))
            (assoc istate key (into #{} val))))
        state
        keys))
    {}
    allergens-with-foods))

(defn remove-ok-candidates [candidates ok-ing]
  (reduce (fn [prev-map [allergen curr-candidates]]
            (if (= #{ok-ing} curr-candidates)
              prev-map
              (assoc prev-map allergen (remove #{ok-ing} curr-candidates))))
          {}
          candidates))

(defn allergen->ingredient [allergens-map]
  (loop [curr-candidates allergens-map results {}]
    (if (every? #(empty? %) (vals curr-candidates))
      results
      (let [[ok-allergen ok-ing] (first (filter #(= 1 (count (val %))) curr-candidates))]
        (recur (remove-ok-candidates curr-candidates (first ok-ing))
               (assoc results ok-allergen (first ok-ing)))))))

(def all-allergens
  (into #{} (vals (allergen->ingredient allergens-map))))

;; Part 1
(->> allergens-with-foods
     (map second)
     flatten
     (filter #(not (contains? all-allergens %)))
     count)

;; Part 2
(->> allergens-map
     allergen->ingredient
     (sort-by first)
     (map second)
     (str/join ","))
