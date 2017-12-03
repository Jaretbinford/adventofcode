(ns adventofcode.core
  (:require [clojure.java.io :as io]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(foo "Jaret")
(+ 1 2)

;;Part 1
;; Grab input into map
(def input (mapv
                (comp read-string str)
                (slurp (io/resource "data"))))

;; make
(partition 2 (conj (vec input) (first input)))

;;Tests

(def samp4 [9 1 2 1 2 1 2 9])

(->> (partition 2 1 (conj (vec samp4) (last samp4)))
     (filter (partial apply =)))

;;Count repeating digits
(defn countstuff [i]
  "maps matching repeating digits in a sequence and sums"
(->> (partition 2 1 (conj (vec i) (first i)))
     (filter (partial apply =))
     (map first)
     (apply +)))

(comment
  (countstuff [1 2 3 4])
  (countstuff [1 1 3 4])
  (countstuff [1 1 1 1])
  (countstuff [9 1 2 1 2 1 2 9]))

(countstuff input)
;;995!

;;Part 2

(def samp4 [9 1 2 1 2 1 2 9])

(->> (map
       vector
       samp4
       (drop (/ (clojure.core/count samp4) 2) (cycle samp4)))
     (filter (partial apply =))
     (map first)
     (apply +))

(defn countstuffagain [i]
  (->> (map
         vector
         i
         (drop (/ (clojure.core/count i) 2) (cycle i)))
       (filter (partial apply =))
       (map first)
       (apply +)))

(countstuffagain input)


;; Day 2!

(ns adventofcode.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input (->>
            (io/resource "data2")
            io/reader
            line-seq
            (mapv (comp (partial mapv read-string)
                        #(string/split % #"\s+")))))

(defn checksum [row]
  (->> ((juxt (partial reduce min) (partial reduce max)) row)
       (apply -)
       (Math/abs)))

(apply + (map checksum input))


(defn checksumagain [row]
  (first (for [a row
               b row
               :when (and (not= a b)
                          (= 0 (rem a b)))]
           (/ a b))))


(apply + (map checksumagain input))









