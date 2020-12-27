(ns aoc.day22)

(def p1-cards
  [14 6 21 10 1 33 7 13 25 8 17 11 28 27 50 2 35 49 19 46 3 38 23 5 43])

(def p2-cards
  [18 9 12 39 48 24 32 45 47 41 40 15 22 36 30 26 42 34 20 16 4 31 37 44 29])

(defn simulate-combat [cards1 cards2]
  (cond
    (empty? cards1) [:p2 cards2]
    (empty? cards2) [:p1 cards1]
    :else (let [top1 (first cards1)
                top2 (first cards2)]
            (if (< top1 top2)
              (recur (rest cards1) (concat (rest cards2) [top2 top1]))
              (recur (concat (rest cards1) [top1 top2]) (rest cards2))))))

(defn calc-ans [combat p1-cards p2-cards]
  (->> (combat p1-cards p2-cards)
       second
       reverse
       (map-indexed #(* (inc %1) %2))
       (apply +)))

(defn simulate-recursive-combat
  ([cards1 cards2]
   (simulate-recursive-combat cards1 cards2 #{}))
  ([cards1 cards2 visited]
   (cond
     (nil? cards1) [:p2 cards2]
     (nil? cards2) [:p1 cards1]
     (visited [cards1 cards2]) [:p1 cards1]
     :else (let [[top1 & rest1] cards1
                 [top2 & rest2] cards2
                 round-winner (if (and (<= top1 (count rest1)) (<= top2 (count rest2)))
                                (first (simulate-recursive-combat
                                         (take top1 rest1)
                                         (take top2 rest2)))
                                (if (> top1 top2) :p1 :p2))
                 visited* (conj visited [cards1 cards2])]
             (case round-winner
               :p1 (recur (concat rest1 [top1 top2]) rest2 visited*)
               :p2 (recur rest1 (concat rest2 [top2 top1]) visited*))))))

;; Part 1
(calc-ans simulate-combat p1-cards p2-cards)
;; Part 2
(calc-ans simulate-recursive-combat p1-cards p2-cards)