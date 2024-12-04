(ns adventofcode24.day1.impl
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn lines []
  (with-open [r (io/reader (io/resource "input/day1.txt"))]
    (->> (line-seq r)
        ;; (take 2)
         (mapv identity))))

(defn parse-line [s]
  (str/split s #"\s+"))

(defn distance [[one two]]
  (abs (- one two)))

(defn with-distance [tuple]
  (let [[one two] tuple]
    {:locationID-one one
     :locationID-two two
     :distance (distance tuple)}))

;; xs has following form [["1" "3"] ["4" "2"]]
(defn to-lists-fn [m [left right]]
  (let [left-id (parse-long left)
        right-id (parse-long right)]
    (-> m
        (update :left conj left-id)
        (update :right conj right-id))))

(defn as-lists [lines]
  (->> lines
       (mapv parse-line)
       (reduce to-lists-fn {:left [] :right []})))

(comment

  ;; implementation down below
  (def part1-answer 1506483)
  (let [{:keys [left right] :as _res} (as-lists (lines))
        #_#_sorted-res (-> res
                           (update :one sort)
                           (update :two sort))]
    (->> (mapv vector (sort left) (sort right))
         ;; (mapv (comp :distance with-distance))
         (mapv distance)
         (reduce + 0)))

  \n)
