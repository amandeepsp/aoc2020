(ns aoc.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def operator-precedence {'+ 1 '* 1})

(defn infix-parse [expr]
  (if-not (seq? expr)
    expr
    (loop [val-stack ()
           op-stack ()
           [opd op & expr] expr]
      (let [priority (if op (operator-precedence op) ##-Inf)
            [popped-ops unpopped-ops] (split-with
                                        #(>= (operator-precedence %) priority)
                                        op-stack)
            val-stack (reduce
                        (fn [[right left & vals] op]
                          (cons (list op left right) vals))
                        (cons (infix-parse opd) val-stack)
                        popped-ops)]
        (if-not op
          (first val-stack)
          (recur val-stack (cons op unpopped-ops) expr))))))


(defn part-1 [file-name]
  (->> (io/resource file-name)
       (slurp)
       (str/split-lines)
       (map #(str "(" % ")"))
       (mapv #(eval (infix-parse (read-string %))))
       (reduce +)))

(prn (part-1 "day18.txt"))

(def operator-precedence* {'+ 2 '* 1})

(with-redefs [operator-precedence operator-precedence*]
  (prn (part-1 "day18.txt")))