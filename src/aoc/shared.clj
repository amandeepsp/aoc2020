(ns aoc.shared
  (:require [clojure.string :as str]))

(defn file-lines
  [file-name]
  (str/split-lines (slurp file-name)))