(ns aoc.day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [file-name]
  (->> (io/resource file-name)
       (slurp)
       (str/split-lines)
       (mapv #(apply vector (seq %)))))

(def seat-arr (parse-input "day11.txt"))

(defn sw [i j] [(dec i) (dec j)])
(defn s [i j] [(dec i) j])
(defn se [i j] [(dec i) (inc j)])
(defn e [i j] [i (inc j)])
(defn w [i j] [i (dec j)])
(defn nw [i j] [(inc i) (dec j)])
(defn n [i j] [(inc i) j])
(defn ne [i j] [(inc i) (inc j)])

(defn valid-neighbors [arr i j]
  (let [height (count arr)
        width (count (first arr))
        neighbors [(e i j) (ne i j) (n i j) (nw i j)
                   (w i j) (sw i j) (s i j) (se i j)]]
    (->> neighbors
         (filter (fn [[i j]]
                   (and (<= 0 i (dec height))
                        (<= 0 j (dec width)))))
         (map (fn [[i j]]
                (get-in arr [i j]))))))

(defn search-seat [arr init-i init-j dir-function]
  (loop [[i j] (dir-function init-i init-j)]
    (cond
      (not (and (<= 0 i (dec (count arr)))
                (<= 0 j (dec (count (first arr))))))
      nil

      (not= \. (get-in arr [i j]))
      (get-in arr [i j])

      :else
      (recur (dir-function i j)))))

(defn modified-valid-neighbors [arr i j]
  (let [search-fn (partial search-seat arr i j)
        neighbors [(search-fn e) (search-fn ne) (search-fn n) (search-fn nw)
                   (search-fn w) (search-fn sw) (search-fn s) (search-fn se)]]
    (filter (complement nil?) neighbors)))

(defn occupied-neighbors [arr valid-neighbors-strategy i j]
  (count (filter #{\#}
                 (valid-neighbors-strategy arr i j))))

(defn produce-modifications
  [valid-neighbors-strategy
   filled-seats-threshold
   seat-arr]
  (for [[i row] (map-indexed list seat-arr)
        [j cell] (map-indexed list row)
        :when (not= \. cell)]
    (let [num-neighbors (partial
                          occupied-neighbors
                          seat-arr
                          valid-neighbors-strategy)]
      (cond
        (and (= cell \L)
             (= 0 (num-neighbors i j)))
        [i j \#]

        (and (= cell \#)
             (>= (num-neighbors i j) filled-seats-threshold))
        [i j \L]))))

(defn produce-nex-state* [curr-state find-modifications]
  (->> curr-state
       (find-modifications)
       (filter (complement nil?))
       (reduce (fn [state [i j val]]
                 (assoc-in state [i j] val))
               curr-state)))

(defn produce-next-state
  [valid-neighbors-strategy
   filled-seats-threshold
   curr-state]
  (let [modifications-generator (partial
                                  produce-modifications
                                  valid-neighbors-strategy
                                  filled-seats-threshold)
        next-state (produce-nex-state*
                     curr-state
                     modifications-generator)]
    (if (= next-state curr-state)
      nil
      next-state)))

(defn find-stable-seats
  [valid-neighbors-strategy
   filled-seats-threshold
   init-seats]
  (let [next-state-generator (partial
                               produce-next-state
                               valid-neighbors-strategy
                               filled-seats-threshold)]
    (->> init-seats
         (iterate next-state-generator)
         (take-while (complement nil?))
         (last))))

(defn count-occupied [seats]
  (apply + (for [row seats
                 cell row]
             (if (= cell \#) 1 0))))

(prn (count-occupied (find-stable-seats valid-neighbors 4 seat-arr))) ;;Part-1
(prn (count-occupied (find-stable-seats modified-valid-neighbors 5 seat-arr))) ;;Part-2