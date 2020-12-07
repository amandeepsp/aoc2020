(ns aoc.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [[_ outer-bag] (re-find #"(.*) bags contain" line)
        inner-bags (map (fn [[_ num bag]]
                          (vector (Integer/parseInt num) bag))
                        (re-seq #"(\d+) (\D+) bag[s]?" line))]
    (vector outer-bag inner-bags)))

(defn parse-input [file-name]
  (->> (slurp (io/resource file-name))
       (str/split-lines)
       (map parse-line)
       (into {})))

(defn any-true? [coll]
  (reduce (fn [acc elem]
            (or acc elem))
          false coll))

(defn can-contain? [bag-map outer-bag inner-bag]
  (let [bag-relation (get bag-map outer-bag)]
    (cond (empty? bag-relation) false
          (some #{inner-bag} (map second bag-relation)) true
          :else (any-true? (map #(can-contain? bag-map % inner-bag)
                                (map second bag-relation))))))

(defn count-bags [bag-map bag]
  (let [bag-relation (get bag-map bag)]
    (if (empty? bag-relation)
      0
      (reduce + (map (fn [[num child-bag]]
                       (* num (inc (count-bags bag-map child-bag))))
                     bag-relation)))))

(defn part-1 [file-name]
  (let [bag-map (parse-input file-name)]
    (->> (keys bag-map)
         (filter #(can-contain? bag-map % "shiny gold"))
         (count))))

(defn part-2 [file-name]
  (let [bag-map (parse-input file-name)]
    (count-bags bag-map "shiny gold")))

(prn 'part-1 (part-1 "day7.txt"))
(prn 'part-2 (part-2 "day7.txt"))

