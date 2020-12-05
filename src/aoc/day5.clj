(ns aoc.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn bin->num [str]
  (Integer/parseInt str 2))

(defn num-pos [pos-str]
  (-> pos-str
      (str/replace #"[B|R]" "1")
      (str/replace #"[F|L]" "0")
      (bin->num)))

(defn parse-input [file-name]
  (str/split-lines (slurp (io/resource file-name))))

(defn part-1 [file-name]
  (let [positions (parse-input file-name)]
    (reduce max (map num-pos positions))))

(defn part-2 [file-name]
  (let [positions (map num-pos (parse-input file-name))
        num-pos (count positions)
        min-pos (reduce min positions)
        pos-xor (apply bit-xor positions)
        nums-xor (apply bit-xor (range min-pos (+ min-pos num-pos 1)))]
    (bit-xor pos-xor nums-xor)))