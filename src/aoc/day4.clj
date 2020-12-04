(ns aoc.day4
  (:require [clojure.string :as str]))

(def mandatory-keys ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn parse-input [str]
  (->> (str/split str #"\n\n")
       (map (fn [str]
              (->> (str/split str #"[\n| ]")
                   (map #(str/split % #":"))
                   (into {}))))))

(defn valid-keys? [passport]
  (every? passport mandatory-keys))

(defn part-1 [file-name]
  (let [passports (parse-input (slurp file-name))]
    (count (filter valid-keys? passports))))

(defn valid-height? [height-str]
  (let [[_ height unit] (re-find #"(\d+)(in|cm)" height-str)]
    (case unit
      "cm" (<= 150 (Integer/parseInt height) 193)
      "in" (<= 59 (Integer/parseInt height) 76)
      false)))

(defn valid-color? [color-str]
  (re-matches #"^#[0-9a-f]{6}$" color-str))

(def valid-eye-color-values #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn valid-eye-color? [eye-color]
  (contains? valid-eye-color-values eye-color))

(defn valid-pid? [str-pid]
  (re-matches #"^\d{9}$" str-pid))

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