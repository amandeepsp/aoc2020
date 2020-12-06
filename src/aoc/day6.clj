(ns aoc.day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [file-name]
  (-> (slurp (io/resource file-name))
      (str/split #"\R\R")
      (->> (map str/split-lines))))

(defn group-count-any [group]
  (count (into #{} (str/join "" group))))

(defn group-count-all [group]
  (let [ques-freqs (frequencies (str/join "" group))]
    (count (filter (fn [[_ val]]
                     (= val (count group)))
                   ques-freqs))))

(defn count-ques [file-name strategy]
  (let [groups (parse-input file-name)]
    (reduce + (map strategy groups))))

(defn part-1 [file-name]
  (count-ques file-name group-count-any))

(defn part-2 [file-name]
  (count-ques file-name group-count-all))
