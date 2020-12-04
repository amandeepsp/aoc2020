(ns aoc.day4
  (:require [clojure.string :as str]))

(def mandatory-keys ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn parse-input [str]
  (->> (str/split str #"\n\n")
       (map (fn [str]
              (->> (str/split str #"[\n| ]")
                   (map #(str/split % #":"))
                   (reduce #(assoc %1 (first %2) (second %2)) {}))))))

(defn valid-keys? [passport]
  (every? passport mandatory-keys))

(defn part-1 [file-name]
  (let [passports (parse-input (slurp file-name))]
    (count (filter valid-keys? passports))))

(defn parse-height [height-str suffix]
  (Integer/parseInt
    (.substring height-str 0 (str/index-of height-str suffix))))

(defn valid-height? [height-str]
  (cond
    (str/ends-with? height-str "cm") (<= 150 (parse-height height-str "cm") 193)
    (str/ends-with? height-str "in") (<= 59 (parse-height height-str "in") 76)
    :else false))

(defn valid-color? [color-str]
  (if-not (str/starts-with? color-str "#")
    false
    (let [color-values (.substring color-str 1)]
      (every? (fn [char-val]
                (or (Character/isDigit char-val)
                    (<= (int \a) (int char-val) (int \f))))
              color-values))))

(def valid-eye-color-values #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn valid-eye-color? [eye-color]
  (contains? valid-eye-color-values eye-color))

(defn valid-pid? [str-pid]
  (and (= 9 (count str-pid))
       (every? (fn [char]
                 (Character/isDigit char)) str-pid)))

(defn valid-value? [[key val]]
  (case key
    "byr" (<= 1920 (Integer/parseInt val) 2002)
    "iyr" (<= 2010 (Integer/parseInt val) 2020)
    "eyr" (<= 2020 (Integer/parseInt val) 2030)
    "hgt" (valid-height? val)
    "hcl" (valid-color? val)
    "ecl" (valid-eye-color? val)
    "pid" (valid-pid? val)
    "cid" true))

(defn valid-values? [passport]
  (every? valid-value? passport))

(defn part-2 [file-name]
  (let [passports (parse-input (slurp file-name))]
    (count (filter (fn [passport]
                     (and (valid-keys? passport)
                          (valid-values? passport)))
                   passports))))