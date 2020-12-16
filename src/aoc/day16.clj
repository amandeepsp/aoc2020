(ns aoc.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.shared :as shared]))

(defn str->int [str]
  (Long/parseLong str))

(defn parse-ticket [str-ticket]
  (->> (str/split str-ticket #",")
       (mapv #(Integer/parseInt %))))

(defn parse-range [range-str]
  (let [[_ type start1 end1 start2 end2] (re-find #"(\D+): (\d+)-(\d+) or (\d+)-(\d+)" range-str)]
    [type [{:start (str->int start1)
            :end   (str->int end1)}
           {:start (str->int start2)
            :end   (str->int end2)}]]))

(defn parse-input [file-name]
  (let [[ranges-str my-ticket-str tickets-str] (-> (io/resource file-name)
                                                   (slurp)
                                                   (str/split #"\R\R"))
        my-ticket (parse-ticket (second (str/split-lines my-ticket-str)))
        tickets (map parse-ticket (rest (str/split-lines tickets-str)))
        ranges (into {} (map parse-range (str/split-lines ranges-str)))]
    {:my-ticket my-ticket :tickets tickets :ranges ranges}))

(def ticket-data (parse-input "day16.txt"))

(defn in-range? [num {:keys [start end]}]
  (<= start num end))

(defn in-any-range? [range-coll item]
  (shared/any-true? (map (partial in-range? item) range-coll)))

(defn error-rate [items ranges]
  (apply +
         (filter (complement (partial in-any-range? ranges)) items)))
;;Part 1
(error-rate (flatten (:tickets ticket-data))
            (flatten (vals (:ranges ticket-data))))

(defn filter-valid [ticket-data]
  (let [ranges (flatten (vals (:ranges ticket-data)))
        valid-tickets (filter (fn [ticket]
                                (every? (partial in-any-range? ranges) ticket))
                              (:tickets ticket-data))]
    (assoc ticket-data :tickets valid-tickets)))

(defn every-in-ranges? [[range1 range2] coll]
  (every? (fn [item]
            (or (in-range? item range1)
                (in-range? item range2)))
          coll))

(defn get-range-keys [range-data coll]
  (map first (filter (fn [[_ ranges]]
                       (every-in-ranges? ranges coll)) range-data)))

(def valid-ticket-data (filter-valid ticket-data))

(def my-ticket (:my-ticket valid-ticket-data))
(def tickets (conj (:tickets valid-ticket-data) my-ticket))
(def ranges (:ranges valid-ticket-data))

(def candidates (into {} (for [i (range (count (first tickets)))]
                           [i (get-range-keys ranges (map #(nth % i) tickets))])))

(defn remove-ok-candidates [candidates ok-key]
  (reduce (fn [prev-map [col keys]]
            (if (= (list ok-key) keys)
              prev-map
              (assoc prev-map col (remove #{ok-key} keys))))
          {}
          candidates))

(def key-col-mappings
  (loop [curr-candidates candidates results {}]
    (if (every? #(empty? (second %)) curr-candidates)
      results
      (let [[ok-col [ok-key]] (first (filter #(= 1 (count (second %))) curr-candidates))]
        (recur (remove-ok-candidates curr-candidates ok-key)
               (assoc results ok-key ok-col))))))
;;Part 2
(->> key-col-mappings
     (filter #(str/starts-with? (first %) "departure"))
     (map second)
     (map #(nth my-ticket %))
     (apply *))