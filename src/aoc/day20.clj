(ns aoc.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [aoc.shared :refer [str->int]]))

;; Solution blatantly copied from @callum-oakley

(def data
  (map str/split-lines (str/split (slurp (io/resource "day20.txt")) #"\r?\n\r?\n")))

(defn re-index [tile f]
  (into []
        (for [y (range (count tile))]
          (into []
                (for [x (range (count (first tile)))]
                  (get-in tile (f [y x])))))))

(defn rotations-and-reflections [t]
  (let [r #(re-index % (fn [[y x]] [(- (count t) x 1) y]))
        f #(re-index % (fn [[y x]] [x y]))]
    [t
     (r t)
     (r (r t))
     (r (r (r t)))
     (f t)
     (f (r t))
     (f (r (r t)))
     (f (r (r (r t))))]))

(defn parse-tile [[title & tile]]
  [(read-string (second (re-find #"(\d+)" title)))
   (let [t (into [] (map #(into [] (seq %)) tile))]
     (rotations-and-reflections t))])

(defn parse [tiles]
  (into {} (map parse-tile tiles)))

(defn border [tile edge]
  (for [i (range 10)]
    (get-in tile
            (case edge
              :north [0 i]
              :east [i 9]
              :south [9 i]
              :west [i 0]))))

(defn adjacent [blueprint]
  (set/difference
    (apply set/union
           (map
             (fn [[y x]]
               #{[(inc y) x]
                 [y (inc x)]
                 [(dec y) x]
                 [y (dec x)]})
             blueprint))
    blueprint))

(defn fits [tiles blueprint [y x] tile-id orientation]
  (let [tile (get-in tiles [tile-id orientation])
        north (blueprint [(dec y) x])
        east (blueprint [y (inc x)])
        south (blueprint [(inc y) x])
        west (blueprint [y (dec x)])]
    (and
      (or (nil? north)
          (= (border tile :north) (border (get-in tiles north) :south)))
      (or (nil? east)
          (= (border tile :east) (border (get-in tiles east) :west)))
      (or (nil? south)
          (= (border tile :south) (border (get-in tiles south) :north)))
      (or (nil? west)
          (= (border tile :west) (border (get-in tiles west) :east))))))

(defn assemble [tiles]
  (loop [blueprint {[0 0] [(first (keys tiles)) 0]}
         remaining (set (rest (keys tiles)))]
    (if (seq remaining)
      (let [[pos tile-id orientation]
            (first (for [pos (adjacent (set (keys blueprint)))
                         tile-id remaining
                         orientation (range 8)
                         :when (fits tiles blueprint pos tile-id orientation)]
                     [pos tile-id orientation]))]
        (recur
          (assoc blueprint pos [tile-id orientation])
          (disj remaining tile-id)))
      blueprint)))

(defn draw [tiles blueprint]
  (let [ymin (apply min (map (fn [[y _]] y) (keys blueprint)))
        xmin (apply min (map (fn [[_ x]] x) (keys blueprint)))
        ymax (apply max (map (fn [[y _]] y) (keys blueprint)))
        xmax (apply max (map (fn [[_ x]] x) (keys blueprint)))]
    (into []
          (for [y (range (* (- (inc ymax) ymin) 8))]
            (into []
                  (for [x (range (* (- (inc xmax) xmin) 8))]
                    (let [tile (get-in tiles (blueprint [(+ ymin (quot y 8))
                                                         (+ xmin (quot x 8))]))]
                      (get-in tile [(+ 1 (rem y 8)) (+ 1 (rem x 8))]))))))))

;;  01234567890123456789
;; 0                  #
;; 1#    ##    ##    ###
;; 2 #  #  #  #  #  #
(def monster
  [[0 18]
   [1 0] [1 5] [1 6] [1 11] [1 12] [1 17] [1 18] [1 19]
   [2 1] [2 4] [2 7] [2 10] [2 13] [2 16]])

(defn count-monsters [image]
  (let [limy (- (count image) 3)
        limx (- (count (first image)) 20)]
    (->> (for [y (range limy) x (range limx)] [y x])
         (filter (fn [[y x]]
                   (every? (fn [[dy dx]]
                             (= (get-in image [(+ y dy) (+ x dx)]) \#))
                           monster)))
         count)))

(defn part-1 [tiles]
  (let [blueprint (assemble tiles)
        ymin (apply min (map (fn [[y _]] y) (keys blueprint)))
        xmin (apply min (map (fn [[_ x]] x) (keys blueprint)))
        ymax (apply max (map (fn [[y _]] y) (keys blueprint)))
        xmax (apply max (map (fn [[_ x]] x) (keys blueprint)))]
    (apply * (for [y [ymin ymax] x [xmin xmax]] ((blueprint [y x]) 0)))))

(defn part-2 [tiles]
  (let [blueprint (assemble tiles)
        image (draw tiles blueprint)
        monsters (apply max
                        (map count-monsters (rotations-and-reflections image)))]
    (- (count (filter #{\#} (flatten image))) (* monsters (count monster)))))

(prn (part-1 (parse data)))
(prn (part-2 (parse data)))