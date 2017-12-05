(ns adventofcode.Day4
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def input (->>
             (io/resource "data3")
             io/reader
             line-seq
             (mapv (comp (partial mapv read-string)
                         #(string/split % #"\s+")))))
(comment
    input
    ;for each input
    (input 1)
    (frequencies (input 1))
    (apply distinct? (input 1)))

(count (filter #(= (count (distinct %)) (count %))
               input))

;; easier to do with a string instead of a vector:
(def input-string (slurp (io/resource "data3")))

(->>
  input-string
  string/split-lines
  (map #(string/split % #" "))
  (map (fn [line] (map sort line)))
  (filter #(= (count %) (count (set %))))
  count)








