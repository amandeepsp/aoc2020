(ns aoc.day25)

(def mod-num 20201227)
(def subject 7)

(def door-key 3248366)
(def card-key 4738476)

(defn loop-size [mod-num subject target]
  (->> (iterate #(mod (* subject %) mod-num) subject)
       (take-while #(not= target %))
       count))

(defn iterate-for [mod-num subject n]
  (->
    (iterate #(mod (* subject %) mod-num) subject)
    (nth n)))

(iterate-for
  mod-num door-key (loop-size mod-num subject card-key))