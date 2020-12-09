(ns aoc.day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [file-name]
  (->> (io/resource file-name)
       (slurp)
       (str/split-lines)
       (mapv #(Long/parseLong %))))

(defn valid-pair? [set-vals [a b]]
  (and (set-vals [b a]) (not= a b)))

(defn can-sum? [coll sum]
  (let [complements (into #{} (map #(vector % (- sum %)) coll))]
    (filter
      (partial valid-pair? complements)
      complements)))

(defn can-diff? [coll diff]
  (let [coll-set (into #{} coll)]
    (->> (filter #(coll-set (+ diff %)) coll)
         (map #(vector
                 (.indexOf coll %)
                 (.indexOf coll (+ diff %)))))))

(defn find-first-invalid [numbers len]
  (loop [index len]
    (cond
      (= index (count numbers)) :no-errors
      (empty? (can-sum?
                (subvec numbers (- index len) index)
                (nth numbers index))) (nth numbers index)
      :else (recur (inc index)))))

(defn part-1 [file-name]
  (let [numbers (parse-input file-name)]
    (find-first-invalid numbers 25)))

(defn part-2 [file-name]
  (let [numbers (parse-input file-name)
        diff (part-1 file-name)
        csum (reduce
               #(conj %1 (+ (last %1) %2))
               (vector (first numbers))
               (next numbers))
        [start end] (first (can-diff? csum diff))
        result-vec (subvec numbers (inc start) (inc end))]
    (prn diff)
    (+ (reduce min result-vec)
       (reduce max result-vec))))

