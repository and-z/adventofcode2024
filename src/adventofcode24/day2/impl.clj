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

(defn make-variant [skip-idx report]
  (loop [[n & more] report
         acc []
         pos 0]
    (if (nil? n)
      acc
      (recur more
             (if (not= pos skip-idx)
               (conj acc n)
               acc)
             (inc pos)))))

(defn variants [report]
  (let [size (count report)]
    (for [idx (range size)]
      (make-variant idx report))))

(defn tolerable? [report]
  (->> (variants report)
       (filter safe?)
       first
       some?))

(def sample-reports
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(comment

  (def part1-answer 559)
  (->> (reports #_sample-reports)
       (filter safe?)
       count)

  (def part2-answer 601)
  (->> (reports #_sample-reports)
       (filter #(or (safe? %) (tolerable? %)))
       count)

  \n)
