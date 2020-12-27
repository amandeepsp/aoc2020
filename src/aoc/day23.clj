(ns aoc.day23)

;; Solution blatantly copied from @callum-oakley
;; couldn't optimize part 2 with persistent collections

(def labels [4 9 6 1 3 8 5 2 7])

(defn initial-state [cups]
  {:next-cup (->> (partition 2 1 cups)
                  (concat [[0 nil] [(last cups) (first cups)]])
                  (sort-by first)
                  (mapv second)
                  transient)
   :current (first cups)
   :maxcup (apply max cups)})

(defn move [{:keys [next-cup current maxcup] :as state}]
  ;; ... current a b c d ... destination e ...
  (let [a (next-cup current) b (next-cup a) c (next-cup b) d (next-cup c)
        destination (->> (iterate #(if (= % 1) maxcup (dec %)) current)
                         rest
                         (some #(when-not (#{a b c} %) %)))
        e (next-cup destination)]
    (assoc state
      :next-cup (assoc! next-cup current d destination a c e)
      :current d)))

(defn game [n cups]
  (:next-cup (first (drop n (iterate move (initial-state cups))))))

(defn part-1 [cups]
  (let [next-cup (game 100 cups)]
    (take (dec (count cups)) (rest (iterate next-cup 1)))))

(defn part-2 [cups]
  (let [next-cup (game 10000000 (concat cups (range 10 1000001)))
        a (next-cup 1) b (next-cup a)]
    (* a b)))

(part-2 labels)