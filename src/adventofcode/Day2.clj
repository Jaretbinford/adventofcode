(ns adventofcode.Day2
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
