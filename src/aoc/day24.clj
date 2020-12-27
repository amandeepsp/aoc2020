(ns aoc.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def dirs ["e" "se" "sw" "w" "nw" "ne"])

(defn parse-line [line]
  (map first (re-seq #"(e|se|sw|w|nw|ne)" line)))

(defn parse-input [file-name]
  (->> (io/resource file-name)
       slurp
       str/split-lines
       (map parse-line)))


(defn next-coord
  "Axial coordinates (q r)
  adopted from https://www.redblobgames.com/grids/hexagons/"
  [[q r] dir]
  (case dir
    "e"  [(inc q) r      ]
    "se" [q       (inc r)]
    "sw" [(dec q) (inc r)]
    "w"  [(dec q) r      ]
    "nw" [q       (dec r)]
    "ne" [(inc q) (dec r)]))

(defn black-tiles [instructions]
  (reduce (fn [acc coords]
            (if (contains? acc coords)
              (disj acc coords)
              (conj acc coords)))
          #{}
          instructions))

(defn neighbors [coord]
  (map next-coord (repeat coord) dirs))

;;Part 1
(def init-black-tiles
  (->> (parse-input "day24.txt")
       (map #(reduce next-coord [0 0] %))
       black-tiles))

(prn (count init-black-tiles))

(def init-state (into {} (map #(vector % :black) init-black-tiles)))

(defn count-neighbors [cell cells state]
  (count (filter #(= % state) (map #(get cells % :white) (neighbors cell)))))

(defn apply-rule [cells cell]
  (let [curr-state (get cells cell :white)
        black-neighbors (count-neighbors cell cells :black)]
    (cond
      (and (= curr-state :black) (or (zero? black-neighbors) (> black-neighbors 2))) :white
      (and (= curr-state :white) (= 2 black-neighbors)) :black
      :else curr-state)))

(defn grow-map [cells]
  (let [grown-map (->> (keys cells)
                       (mapcat neighbors)
                       (map #(vector % :white))
                       (into {}))]
    (merge grown-map cells)))

(defn next-state [cells]
  (let [grown-map (grow-map cells)]
    (reduce (fn [cells-state cell]
              (assoc cells-state cell (apply-rule grown-map cell)))
            {}
            (keys grown-map))))

;; Part 2
(prn (->> init-state
     (iterate next-state)
     (map (fn [curr-state]
            (count (filter #(= :black (second %)) curr-state))))
     (drop 100)
     first))