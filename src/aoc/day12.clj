(ns aoc.day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-line [line]
  [(first line)
   (-> (rest line)
       (str/join)
       (Integer/parseInt))])

(defn parse-input [file-name]
  (->> (io/resource file-name)
       (slurp)
       (str/split-lines)
       (map parse-line)))

(def steps (parse-input "day12.txt"))

(def  axis-left [\E \N \W \S])

(defn turn-left [dir degree]
  {:pre [(<= 0 (.indexOf axis-left dir) (dec (count axis-left)))
         (zero? (mod degree 90))]}
  (let [new-axis-idx (mod (+ (.indexOf axis-left dir) (/ degree 90))
                          (count axis-left))]
    (nth axis-left new-axis-idx)))

(defn turn-right [dir degree]
  (turn-left dir (- degree)))


(defn next-state [[x y angle] [dir val]]
  (case dir
    \N [x (+ y val) angle]
    \E [(+ x val) y angle]
    \W [(- x val) y angle]
    \S [x (- y val) angle]
    \L [x y (turn-left angle val)]
    \R [x y (turn-right angle val)]
    \F (next-state [x y angle] [angle val])))

(def reduced-state
  (reduce (fn [state step]
            (next-state state step))
          [0 0 \E]
          steps))
;; Part 1
(prn (+ (Math/abs (first reduced-state))
        (Math/abs (second reduced-state))))

(defn turn-left* [[dx dy] degree]
  {:pre [(zero? (mod degree 90))]}
  (case (mod (/ degree 90) (count axis-left))
    0 [dx dy]
    1 [(- dy) dx]
    2 [(- dx) (- dy)]
    3 [dy (- dx)]))

(defn turn-right* [[dx dy] degree]
  (turn-left* [dx dy] (- degree)))

(defn next-state* [[x y [dx dy]] [dir val]]
  (case dir
    \N [x y [dx (+ dy val)]]
    \E [x y [(+ dx val) dy]]
    \W [x y [(- dx val) dy]]
    \S [x y [dx (- dy val)]]
    \L [x y (turn-left* [dx dy] val)]
    \R [x y (turn-right* [dx dy] val)]
    \F [(+ x (* val dx)) (+ y (* val dy)) [dx dy]]))

(def reduced-state*
  (reduce next-state* [0 0 [10 1]] steps))

;; Part 2
(prn (+ (Math/abs (first reduced-state*))
        (Math/abs (second reduced-state*))))