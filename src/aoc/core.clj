(ns aoc.core
  (:require [aoc.day1]
            [aoc.day2]
            [aoc.day3]
            [aoc.day4]
            [aoc.day5]))

(defn -main
  "Used to dispatch tasks from the command line.
  
  lein run d01.p1"
  [part]
  (case part
    "d01.p2" (println (aoc.day1/part-2 "day1.txt"))
    "d01.p1" (println (aoc.day1/part-1 "day1.txt"))
    "d02.p1" (println (aoc.day2/part-1 "day2.txt"))
    "d02.p2" (println (aoc.day2/part-2 "day2.txt"))
    "d03.p1" (println (aoc.day3/part-1 "day3.txt"))
    "d03.p2" (println (aoc.day3/part-2 "day3.txt"))
    "d04.p1" (println (aoc.day4/part-1 "day4.txt"))
    "d04.p2" (println (aoc.day4/part-2 "day4.txt"))
    "d05.p1" (println (aoc.day5/part-1 "day5.txt"))
    "d05.p2" (println (aoc.day5/part-2 "day5.txt"))
    (println "not found")))
