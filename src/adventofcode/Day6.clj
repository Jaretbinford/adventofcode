(ns adventofcode.Day6
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))


;The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks, so it is chosen for redistribution.
;Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on, the 7 blocks are spread out over the memory banks. The fourth, first, and second banks get two blocks each, and the third bank gets one back. The final result looks like this: 2 4 1 2.
;Next, the second bank is chosen because it contains the most blocks (four). Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
;Now, there is a tie between the first and fourth memory banks, both of which have three blocks. The first bank wins the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
;The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
;The third bank is chosen, and the same thing happens: 2 4 1 2.


(def test-data [0 2 7 0])

;Need to take largest column (bank) and then distribute to each column.  Start again looking for first repeat.

(def data (->>
             (io/resource "data5")
             io/reader
             line-seq
             (mapv (comp (partial mapv read-string)

                         #(string/split % #"\s+")))))

data
(def data2 (data 0))

(def maxcol (apply max data2))

maxcol

(assoc data2 (first (positions #{maxcol} data2)) 0)

(defn day6 [in]
  (loop [seen #{}
         n-cycle 0
         memory in]
    (let [[max-idx max-count] (reduce
                                (fn [[midx mc :as ma] [ci cc :as ca]]
                                  (if (< mc cc)
                                    ca
                                    ma))
                                [-1 -1]
                                (map list (range) memory))]
      (let [next-memory (reduce
                          (fn [m idx]
                            (update m (mod idx (count m)) inc))
                          (assoc memory max-idx 0)
                          (range (inc max-idx) (+ 1 max-idx max-count)))]
        (if (contains? seen next-memory)
          (inc n-cycle)
          (recur (conj seen next-memory)
                 (inc n-cycle)
                 next-memory))))))


(defn day6-2 [in]
  (loop [seen-at {}
         n-cycle 0
         memory in]
    (let [[max-idx max-count] (reduce
                                (fn [[midx mc :as ma] [ci cc :as ca]]
                                  (if (< mc cc)
                                    ca
                                    ma))
                                [-1 -1]
                                (map list (range) memory))]
      (let [next-memory (reduce
                          (fn [m idx]
                            (update m (mod idx (count m)) inc))
                          (assoc memory max-idx 0)
                          (range (inc max-idx) (+ 1 max-idx max-count)))]
        (if-let [first-seen (get seen-at next-memory)]
          (- (inc n-cycle) first-seen)
          (recur (assoc seen-at next-memory (inc n-cycle))
                 (inc n-cycle)
                 next-memory))))))


(day6 [4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3])
(day6-2 [4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3])






