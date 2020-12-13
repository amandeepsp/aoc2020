(ns aoc.day13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [file-name]
  (let [[time ids] (->> (io/resource file-name)
                        (slurp)
                        (str/split-lines))
        valid-ids (->> (str/split ids #",")
                       (filter #(not= "x" %))
                       (map #(Integer/parseInt %)))
        earliest-time (Integer/parseInt time)]
    {:time earliest-time :bus-ids valid-ids}))

(def bus-data (parse-input "day13.txt"))

(def next-times
  (map
    (fn [bus-id]
      {:id        bus-id
       :next-time (- bus-id
                     (mod (:time bus-data) bus-id))})
    (:bus-ids bus-data)))

;; Part 1
(let [min-next-time (apply (partial min-key :next-time) next-times)]
  (* (:id min-next-time) (:next-time min-next-time)))

(defn parse-input2 [file-name]
  (let [[_ ids] (->> (io/resource file-name)
                     (slurp)
                     (str/split-lines))]
    (->> (str/split ids #",")
         (map-indexed #(list %1 %2))
         (filter #(not= "x" (second %)))
         (map #(list (first %) (Long/parseLong (second %)))))))

(def bus-data2 (parse-input2 "day13.txt"))

;; Part 2 (first element)

(defn increase-jump [[curr-time jump-val] [diff bus-id]]
  (loop [time curr-time]
    (if (= 0 (mod (+ time diff) bus-id))
      [time (* jump-val bus-id)]
      (recur (+ time jump-val)))))

(reduce
  increase-jump
  [0 (second (first bus-data2))]
  (next bus-data2))


