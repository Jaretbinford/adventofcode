(ns adventofcode.Day5
  (:require [clojure.java.io :as io]))


;For example, consider the following list of jump offsets:
;
;0
;3
;0
;1
;-3
;Positive jumps ("forward") move downward; negative jumps move upward. For legibility in this example, these offset values will be written all on one line, with the current instruction marked in parentheses. The following steps would be taken before an exit is found:
;
;(0) 3  0  1  -3  - before we have taken any steps.
;(1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
;2 (3) 0  1  -3  - step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
;2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
;2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
;2  5  0  1  -2  - jump 4 steps forward, escaping the maze.
;In this example, the exit is reached in 5 steps.

(def data (->> (io/resource "data4")
               io/reader
               line-seq
               (mapv read-string)))

(defn finished? [size]
  (fn [[position _ _]]
    (or (neg? position) (>= position size))))

(defn part-1 [[position instructions step]]
  (let [content (get instructions position)
        new-instructions (update instructions position inc)
        new-position (+ position content)]
    [new-position new-instructions (inc step)]))

(defn solve [jump]
  (let [in data
        finished? (finished? (count in))
        start-state [0 in 0]]
    (->> (iterate jump start-state)
         (drop-while (complement finished?))
         first)))

(solve part-1)
;;315613

(defn part-2 [[n instructions step]]
  (let [whatposition (get instructions n)
        offset (if (>= whatposition 3) dec inc)
        new-instructions (update instructions n offset)
        new-n (+ n whatposition)]
    [new-n new-instructions (inc step)]))

(solve part-2)
;;22570529









