(ns aoc.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [aoc.shared :as shared]))

(defn parse-rules [rules]
  (let [[num rule-str] (str/split rules #": ")]
    [(shared/str->int num)
     (if-let [[_ end-str] (re-matches #"\".\"" rule-str)]
       (str end-str)
       (let [next-nos (str/split rule-str #" ")
             [first-nos [_ & second-nos]] (split-with #(not= "|" %) next-nos)]
         [(map shared/str->int first-nos)
          (map shared/str->int second-nos)]))]))

(defn parse-file [file-name]
  (let [input (slurp (io/resource file-name))
        [rules data] (str/split input #"\r?\n\r?\n")
        parsed-rules (into {} (map parse-rules (str/split-lines rules)))
        parsed-data (str/split-lines data)]
    [parsed-rules parsed-data]))

(def parsed-data (parse-file "day19.txt"))

(def rules (first parsed-data))
(def data (second parsed-data))

(defn matches? [rules message rules-considering]
  (cond
    (empty? message) (empty? rules-considering)
    (empty? rules-considering) false
    :else
    (let [curr-rule (rules (first rules-considering))]
      (if (string? curr-rule)
        (if-not (= (first message) (first curr-rule))
          false
          (matches? rules (rest message) (rest rules-considering)))
        (shared/any-true?
          (map
            #(matches? rules message (concat % (rest rules-considering)))
            (filter not-empty curr-rule)))))))

;; Part 1 and 2 (change file)
(count (filter #(matches? rules % [0]) data))

;; from a comment by u/Ecstatic-Record6067 via Context Free Grammar
(defn with-instaparser [file-name]
  (let [input (slurp (io/resource file-name))
        [rules data] (str/split input #"\r?\n\r?\n")
        parser (insta/parser rules :start :0)]
    (->> data
         str/split-lines
         (map parser)
         (filter sequential?)
         (count))))

;; Part 1 and 2 (change file)
(with-instaparser "day19.txt")