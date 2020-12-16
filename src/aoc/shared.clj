(ns aoc.shared
  (:require [clojure.string :as str]))

(defn file-lines
  [file-name]
  (str/split-lines (slurp file-name)))

(defn any-true? [coll]
  (reduce (fn [acc elem]
            (or acc elem))
          false coll))