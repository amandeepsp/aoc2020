(ns aoc.day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn long->bin [num]
  (Long/toBinaryString num))

(defn bin->long [str]
  (Long/parseLong str 2))

(defn parse-inputs [file-name]
  (->> (slurp (io/resource file-name))
       (str/split-lines)
       (map #(re-find #"mem\[(\d+)\] = (\d+)|mask = ([X|1|0]+)" %))
       (map (fn [[_ pos val mask]]
              (if (nil? mask)
                {:pos (Long/parseLong pos) :val (Long/parseLong val)}
                {:mask mask})))))

(defn apply-mask [mask num]
  (let [and-mask (bin->long (str/join (replace {\X \1, \1 \0} mask)))
        or-mask (bin->long (str/join (replace {\X \0} mask)))]
    (bit-or (bit-and and-mask num) or-mask)))

(def instructions (parse-inputs "day14.txt"))

(def memory-snapshot
  (reduce (fn [[memory-map curr-mask] {:keys [pos val mask]}]
            (if-not (nil? mask)
              [memory-map mask]
              [(assoc memory-map pos (apply-mask curr-mask val)) curr-mask]))
          [{} (:mask (first instructions))]
          (next instructions)))

;; Part-1
(apply + (vals (first memory-snapshot)))

(defn zero-pad [str-num cnt]
  (loop [s (str str-num)]
    (if (= (.length s) cnt)
      s
      (recur (str "0" s)))))

(defn positions
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn generate-possible-pos [mask num]
  (let [padded-bits (zero-pad (long->bin num) 36)
        masked-bits (mapv (fn [mask-bit num-bit]
                           (if (= \0 mask-bit)
                             num-bit
                             mask-bit))
                         mask padded-bits)
        floating-indices (positions #(= \X %) masked-bits)
        floating-count (count floating-indices)
        bins (map #(zero-pad (long->bin %) floating-count)
                  (range (Math/pow 2 floating-count)))
        replacements (for [bin bins]
                       (map vector floating-indices bin))]
    (for [replacement replacements]
      (->> replacement
          (reduce (fn [bits [index val]]
                (assoc bits index val))
              masked-bits)
           (str/join)
           (bin->long)))))

(defn assoc-all [map coll val]
  (reduce #(assoc %1 %2 val) map coll))

(def memory-snapshot-part-2
  (reduce (fn [[memory-map curr-mask] {:keys [pos val mask]}]
            (if-not (nil? mask)
              [memory-map mask]
              [(assoc-all memory-map (generate-possible-pos curr-mask pos) val) curr-mask]))
          [{} (:mask (first instructions))]
          (next instructions)))
;;Part 2
(apply + (vals (first memory-snapshot-part-2)))
