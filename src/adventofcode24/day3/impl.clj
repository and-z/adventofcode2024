(ns adventofcode24.day3.impl
  (:require [clojure.java.io :as io]))

(def sample-memory "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn raw-memory []
  (slurp (io/resource "input/day3.txt")))

(defn parse-mul [s]
  (let [[_ x y] (re-find #"mul\((\d{1,3})+,(\d{1,3})+\)" s)]
    {:fn * :x (parse-long x) :y (parse-long y)}))

(defn parse-instructions [s]
  (re-seq #"mul\(\d{1,3},\d{1,3}\)" s))

(comment

  (def part1-answer 187825547)
  (->> (parse-instructions (raw-memory))
       (mapv parse-mul)
       (mapv (fn [{:keys [fn x y]}] (apply fn [x y])))
       (reduce + 0))

  \n)
