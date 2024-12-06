(ns adventofcode24.day4.impl
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn puzzle-input []
  (slurp (io/resource "input/day4.txt")))

;; assume fix line width of N lines
(def sample-input
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def mini-input
  "MSAMX
AMXSX
MSAMA")

(defn prepare-input [s]
  (->> (str/split-lines s)
       (mapv (fn [line] (vec (seq line))))))

;; 2d pos [x y] on this field:
;; [[0 0] [1 0]] -> x-axis (increase)
;; [[0 1] [1 1]]
;; |
;; v y-axis (increase)

(def sample-prepared (prepare-input sample-input))

(defn make-field [prepared]
  (let [field prepared
        width (count (first field))
        heigth (count field)]
    {:fields field :mx (dec width) :my (dec heigth) :width width :heigth heigth}))

(defn ch [[x y] {:keys [mx my fields]}]
  (when (and (<= 0 x mx)
             (<= 0 y my))
    (nth (nth fields y) x)))

(defn char-seq
  "Returns a seq of chars including corresponding [x y] coordinates."
  [prepared]
  (let [{:keys [width heigth] :as field} (make-field prepared)]
    (for [y (range heigth)
          x (range width)]
      {:ch (ch [x y] field)
       :coords [x y]})))

(defn x-seq [chx]
  (->> chx (filter (comp #{\X} :ch))))

(defn move- [steps f p]
  (vec (take steps (iterate f p))))

(def move4 (partial move- 4))

(def move-n
  (partial move4 (fn [[x y]] [x (dec y)])))

(def move-ne
  (partial move4 (fn [[x y]] [(inc x) (dec y)])))

(def move-e
  (partial move4 (fn [[x y]] [(inc x) y])))

(def move-se
  (partial move4 (fn [[x y]] [(inc x) (inc y)])))

(def move-s
  (partial move4 (fn [[x y]] [x (inc y)])))

(def move-sw
  (partial move4 (fn [[x y]] [(dec x) (inc y)])))

(def move-w
  (partial move4 (fn [[x y]] [(dec x) y])))

(def move-nw
  (partial move4 (fn [[x y]] [(dec x) (dec y)])))

(defn directions [{:keys [coords]} {:keys [mx my]}]
  (let [[x y] coords
        word-length (count "XMAS")
        offset (dec word-length)
        point {:x x :y y}
        compass [{:k :n :test (fn [p] (<= offset (:y p))) :cells move-n}
                 {:k :ne
                  :test (fn [p] (and (<= offset (:y p)) (<= (:x p) (- mx offset))))
                  :cells move-ne}
                 {:k :e :test (fn [p] (<= (:x p) (- mx offset))) :cells move-e}
                 {:k :se
                  :test (fn [p] (and (<= (:x p) (- mx offset)) (<= (:y p) (- my offset))))
                  :cells move-se}
                 {:k :s :test (fn [p] (<= (:y p) (- my offset))) :cells move-s}
                 {:k :sw
                  :test (fn [p] (and (<= (:y p) (- my offset)) (<= offset (:x p))))
                  :cells move-sw}
                 {:k :w :test (fn [p] (<= offset (:x p))) :cells move-w}
                 {:k :nw
                  :test (fn [p] (and (<= offset (:x p)) (<= offset (:y p))))
                  :cells move-nw}]]
    (->> compass
         (reduce (fn [acc {:keys [test] :as v}]
                   (if (test point)
                     (conj acc v)
                     acc))
                 []))))

(defn humanize [dir-kw]
  (let [unicode {:n "↑" :ne "↗" :e "→" :se "↘" :s "↓" :sw "↙" :w "←" :nw "↖"}]
    (get unicode dir-kw)))

(defn search->word [m xs]
  (->> xs
       (mapv #(ch % m))
       (apply str)))

(defn find-words [prepared]
  (let [field (make-field prepared)
       start-chars (x-seq (char-seq prepared))
       searches
       (->> start-chars
            (mapcat (fn [{:keys [coords] :as x}]
                      (let [fns (->> (directions x field)
                                     (mapv :cells))]
                        (mapv (fn [f] (f coords)) fns)))))]
   (->> searches
        (mapv (partial search->word field))
        (filter #(= "XMAS" %)))))

(comment
  (def field (prepare-input mini-input))

  ;; visualize possible directions for all found Xs
  (let [prepared (prepare-input sample-input)
        field (make-field prepared)
        start-chars (x-seq (char-seq prepared))]
    (->> start-chars
         (mapcat (fn [x]
                   (->> (directions x field)
                        (mapv (comp humanize :k)))))))

  (count (find-words (prepare-input (puzzle-input) #_sample-input)))

  (x-seq (char-seq (prepare-input mini-input)))

  ;; test predicates
  (let [my 9
        mx 9
        pred (fn [p] (<= (:x p) (- mx 3)))]
    (pred {:x 6 :y 0}))

  \n)
