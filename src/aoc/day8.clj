(ns aoc.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-line [line]
  (let [[opcode val] (str/split line #" ")]
    [(keyword opcode) (Integer/parseInt val)]))

(defn parse-input [file-name]
  (->> (slurp (io/resource file-name))
       (str/split-lines)
       (mapv parse-line)))

(defn run-machine [ops]
  (loop [pc 0 acc 0 visited #{}]
    (cond
      (visited pc) [:loop acc]
      (= pc (count ops)) [:term acc]
      :else (let [[op val] (get ops pc)]
              (case op
                :nop (recur (inc pc) acc (conj visited pc))
                :acc (recur (inc pc) (+ acc val) (conj visited pc))
                :jmp (recur (+ pc val) acc (conj visited pc)))))))

(defn part-1 [file-name]
  (let [ops (parse-input file-name)]
    (run-machine ops)))

(defn run-machine-for-state [ops start state]
  (loop [pc start visited #{}]
    (cond
      (visited pc) (update state :unvisited #(remove visited %))
      (= pc (count ops)) (-> state
                             (update :unvisited #(remove visited %))
                             (update :halting-ops into visited))
      :else (let [[op val] (get ops pc)]
              (case op
                :nop (recur (inc pc) (conj visited pc))
                :acc (recur (inc pc) (conj visited pc))
                :jmp (recur (+ pc val) (conj visited pc)))))))

(defn classify-ops [ops]
  (let [init-state {:halting-ops #{}
                    :unvisited   (range (count ops))}]
    (loop [state init-state]
      (if (empty? (:unvisited state))
        state
        (recur (run-machine-for-state
                 ops
                 (first (:unvisited state))
                 (update state :unvisited next)))))))

(defn correct-ops [ops]
  (let [{:keys [halting-ops]} (classify-ops ops)]
    (loop [index 0]
      (let [[opcode val] (nth ops index)]
        (cond
          (= index (count ops)) ops
          (and (= :jmp opcode) (halting-ops (inc index))) (assoc ops index [:nop val])
          (and (= :nop opcode) (halting-ops (+ index val))) (assoc ops index [:jmp val])
          :else (recur (inc index)))))))

(defn part-2 [file-name]
  (let [ops (parse-input file-name)
        correct-ops (correct-ops ops)]
    (run-machine correct-ops)))

(prn (part-1 "day8.txt"))
;; => 2003

(prn (part-2 "day8.txt"))
;; => 1984



