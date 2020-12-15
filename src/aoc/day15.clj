(ns aoc.day15)

(def starting-numbers [9 12 1 4 17 0 18])

(defn spoken-at [target numbers]
  (let [history (zipmap (butlast numbers) (range 1 (count numbers)))]
    (loop [turn (count numbers), last-spoken (last numbers), hist history]
      (let [first-time? (not (contains? hist last-spoken))
            next-spoken (if first-time? 0 (- turn (hist last-spoken)))]
        (cond
          (= turn target) last-spoken
          :else (recur (inc turn) next-spoken (assoc hist last-spoken turn)))))))

(spoken-at 2020 starting-numbers)
(spoken-at 30000000 starting-numbers) ;; ~ 20 sec