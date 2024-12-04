(ns adventofcode24.day2.impl
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn raw-data []
  (with-open [r (io/reader (io/resource "input/day2.txt"))]
   (->> (line-seq r)
        (mapv identity))))

(defn parse-line [s]
  (->> (str/split s #"\s")
       (mapv parse-long)))

(defn reports
  ([]
   (->> (raw-data)
        (mapv parse-line)))
  ([s]
   (->> (str/split-lines s)
        (mapv parse-line))))

(defn delta [x y]
  (abs (- x y)))

(defn deltas [report]
  (->> report
       (partition 2 1)
       (mapv #(apply delta %))))

(defn accepted-distance? [n]
  (<= 1 n 3))

(defn gradually-increasing? [xs]
  (and (boolean (seq (rest xs)))
       (apply < xs)
       (every? accepted-distance? (deltas xs))))

(defn gradually-decreasing? [xs]
  (and (boolean (seq (rest xs)))
       (apply > xs)
       (every? accepted-distance? (deltas xs))))

(defn safe? [report]
  (or (gradually-decreasing? report)
      (gradually-increasing? report)))

(def sample-reports
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(comment

  (def sample-report "9 7 6 2 1")

  (->> (parse-line sample-report)
       (partition 2 1)
       (mapv #(apply delta %))
       (every? accepted-distance?))

  (def part1-answer 559)
  (->> (reports #_sample-reports)
       (filter safe?)
       count)

  ;; distances
  (every? accepted-distance? [2 1 4 1]) ;; => false
  (every? accepted-distance? [2 1 3 1]) ;; => true

  (= true (gradually-decreasing? [3 2 1]))
  (= true (gradually-decreasing? [5 2 1]))
  (= false (gradually-decreasing? [6 2 1]))
  (= false (gradually-decreasing? [3 2 2]))
  (= false (gradually-decreasing? [1 2 3]))
  (= false (gradually-decreasing? [1]))

  (= false (gradually-increasing? [3 2 1]))
  (= false (gradually-increasing? [1 2 2]))
  (= true  (gradually-increasing? [1 2 3]))
  (= true  (gradually-increasing? [1 2 5]))
  (= false  (gradually-increasing? [1 2 6]))
  (= false (gradually-increasing? [1]))

  \n)
