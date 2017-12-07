(ns adventofcode.Day7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.walk :as w]))

(defn parse-weight [weight-str]
  (Integer/parseInt (clojure.string/replace weight-str #"[()]" "")))

(defn parse-line [line]
  (let [[name weight-str & [arrow & supported]] (s/split line #" ")
        w (parse-weight weight-str)
        supported (when (= "->" arrow)
                    (mapv #(s/replace % #"[, ]" "") supported))]
    {:name name
     :weight w
     :supporting supported}))


(def input
  (map parse-line (s/split-lines (slurp (io/resource "data6")))))


(defn input->map [input]
  (into {} (map #(vector (:name %) %)) input))

;; part 1

(defn solve1 [input]
  (let [supporting (filter :supporting input)
        all-supported (set (mapcat :supporting supporting))]
    (first (filter #(not (all-supported %)) (map :name supporting)))))

(solve1 input)

;; part 2

(defn walk-tally [name-map root-node]
  (tree-seq
    #(:supporting %)
    (fn [parent-node]
      (->> (:supporting parent-node)
           (map
             (fn [node-name]
               (let [node (name-map node-name)]
                 (assoc node
                   :parent (:name parent-node)
                   :tally (->> (walk-tally name-map node)
                               (map :weight)
                               (apply +))))))))
    root-node))

(defn solve2 [input]
  (let [name-map (input->map input)
        head (name-map (solve1 input))
        tallied (walk-tally name-map head)
        unbalanced-discs
        (->> tallied
             (group-by :parent)
             (filter #(apply not= (map :tally (second %)))))
        [unbalanced-parent unbalanced-children]
        (apply min-key
               #(-> % second first :tally)
               unbalanced-discs)
        tally-grouped
        (group-by :tally unbalanced-children)
        [correct-weight _]
        (apply max-key
               #(-> % second count)
               tally-grouped)
        [incorrect-weight [unbalanced-node]]
        (apply min-key
               #(-> % second count)
               tally-grouped)]
    {:correct-weight correct-weight
     :incorrect-weight incorrect-weight
     :unbalanced-node (select-keys unbalanced-node [:name :weight])
     :corrected-weight
     (+ (:weight unbalanced-node)
        (- correct-weight incorrect-weight))}))

(solve2 input)








