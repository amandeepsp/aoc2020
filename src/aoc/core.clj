(ns aoc.core
  (:require [aoc.day1]
            [aoc.day2]
            [aoc.day3]
            [aoc.day4]))

(defn -main
  "Used to dispatch tasks from the command line.
  
  lein run d01.p1"
  [part]
  (case part
    "d01.p1" (println (aoc.day1/part-1 "day-01.txt"))
    "d01.p2" (println (aoc.day1/part-2 "day-01.txt"))
    "d02.p1" (println (aoc.day2/part-1 "day-02.txt"))
    "d02.p2" (println (aoc.day2/part-2 "day-02.txt"))
    "d03.p1" (println (aoc.day3/part-1 "day-03.txt"))
    "d03.p2" (println (aoc.day3/part-2 "day-03.txt"))
    "d04.p1" (println (aoc.day4/part-1 "day-04.txt"))
    "d04.p2" (println (aoc.day4/part-2 "day-04.txt"))
    (println "not found")))
