(ns aoc.day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [file-name]
  (->> (io/resource file-name)
       (slurp)
       (str/split-lines)
       (mapv #(Long/parseLong %))))

(defn valid-sum-pairs [coll sum]
  (let [complements (into #{} (map #(vector % (- sum %)) coll))]
    (filter
      (fn [[a b]]
        (and (complements [b a]) (not= a b)))
      complements)))

(defn valid-diff-pairs [coll diff]
  (let [coll-set (into #{} coll)]
    (->> (filter #(coll-set (+ diff %)) coll)
         (map #(vector
                 (.indexOf coll %)
                 (.indexOf coll (+ diff %)))))))

(defn find-first-invalid [numbers len]
  (loop [index len]
    (cond
      (= index (count numbers)) :no-errors
      (empty? (valid-sum-pairs
                (subvec numbers (- index len) index)
                (nth numbers index))) (nth numbers index)
      :else (recur (inc index)))))

(defn part-1 [file-name]
  (let [numbers (parse-input file-name)]
    (find-first-invalid numbers 25)))

(defn part-2 [file-name]
  (let [numbers (parse-input file-name)
        diff (part-1 file-name)
        csum (reductions + numbers)
        [start end] (first (valid-diff-pairs csum diff))
        result-vec (subvec numbers (inc start) (inc end))]
    (+ (reduce min result-vec) (reduce max result-vec))))

(prn (part-1 "day9.txt"))
;; => 57195069

(prn (part-2 "day9.txt"))
;; => 7409241

