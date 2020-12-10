(ns aoc.day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [file-name]
  (->> (io/resource file-name)
       (slurp)
       (str/split-lines)
       (mapv #(Long/parseLong %))))

(def adapters (sort (parse-input "day10.txt")))

(def adapters* (-> adapters
                   (conj 0)
                   (concat [(+ 3 (last adapters))])))

(def jolt-frequencies
  (frequencies (map - (rest adapters*) adapters*)))

;;Part 1
(* (get jolt-frequencies 1) (get jolt-frequencies 3))

(def adapter-set (into #{} adapters*))

(defn count-ways [value]
  (cond
    (not (adapter-set value)) 0
    (= value 0) 1
    :else (+ (count-ways (dec value))
             (count-ways (- value 2))
             (count-ways (- value 3)))))

(defn count-ways* [value]
  (with-redefs [count-ways (memoize count-ways)]
    (count-ways value)))

;; Part-2
(count-ways* (last adapters*))