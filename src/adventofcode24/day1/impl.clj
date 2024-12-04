(ns adventofcode24.day1.impl
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn lines
  ([s]
   (str/split s #"\n"))
  ([]
   (with-open [r (io/reader (io/resource "input/day1.txt"))]
     (->> (line-seq r)
         ;; (take 100)
          (mapv identity)))))

(defn parse-line [s]
  (str/split s #"\s+"))

(defn distance [[one two]]
  (abs (- one two)))

(defn to-lists-fn [m [left right]]
  (let [left-id (parse-long left)
        right-id (parse-long right)]
    (-> m
        (update :left conj left-id)
        (update :right conj right-id))))

(defn as-lists
  "Turns lines into columns."
  [lines]
  (->> lines
       (mapv parse-line)
       (reduce to-lists-fn {:left [] :right []})))

(defn appearances
  "Calculates how many times number `n` appears in list `xs`"
  [n xs]
  (->> xs
       (reduce
         (fn [acc x]
           (if (= n x)
             (inc acc)
             acc))
         0)))

(defn calc-similarity-score [n right]
  (* n (appearances n right)))

(defn calc-list-similarity-score
  "Calculate similarity score for every item in the `left` list."
  [left right]
  (->> left
       (mapv #(calc-similarity-score % right))
       (reduce + 0)))

(comment

  (def sample
    "3   4
4   3
2   5
1   3
3   9
3   3")

  (lines sample)

  (appearances 4 [0 1 4 5 4 2])
  ;; => 2

  (appearances 6 [0 1 4 5 4 2])
  ;; => 0

  ;; implementation down below
  (def part1-answer 1506483)
  (let [{:keys [left right] :as _res} (as-lists (lines))]
    (->> (mapv vector (sort left) (sort right))
         (mapv distance)
         (reduce + 0)))

  ;; implementation down below
  (def part2-answer 23126924)
  (let [{:keys [left right]} (as-lists (lines))
        #_#_hits (clojure.set/intersection (set left) (set right))]
    (calc-list-similarity-score left right))

  \n)
