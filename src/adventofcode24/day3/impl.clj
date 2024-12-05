(ns adventofcode24.day3.impl
  (:require [clojure.java.io :as io]))

(def sample-memory "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn raw-memory []
  (slurp (io/resource "input/day3.txt")))

(defn digit? [ch]
  (Character/isDigit ch))

(def allowed-chars
  (set (seq "don't()mul,")))

(defn parse-expr [s]
  (when-some [[_ x y] (re-find #"mul\((\d{1,3})+,(\d{1,3})+\)" s)]
    {:f * :x (parse-long x) :y (parse-long y)}))

(comment
  (parse-expr "mul(2,4)")
  (parse-expr "mul(2,)")
  (parse-expr "mul(,4)")

  \n
  )

(defn enable? [s]
  (re-find #"do\(\)" s))
(defn disable? [s]
  (re-find #"don't\(\)" s))

(defn compute-result [instructions]
  (->> instructions
      (mapv (fn [{:keys [f x y]}] (apply f [x y])))
      (reduce + 0)))

(defn parse-instructions
  ([char-seq]
   (parse-instructions {} char-seq))
  ([{:keys [extended?]} char-seq]
   (loop [[c & more] char-seq
          buf []
          state {:parsed []}]
     (cond
       (nil? c)
       state

       (= \) c)
       (let [new-buf (conj buf c)
             buff-str (apply str new-buf)
             expr (parse-expr buff-str)]
       ;; (println buff-str expr)
         (cond
           (and (not (:disabled? state))
                (some? expr))
           (recur more [] (update state :parsed conj expr))

           (and extended? (disable? buff-str))
           (do #_(println "DISABLED")
            (recur more [] (assoc state :disabled? true)))

           (and extended? (enable? buff-str))
           (do #_(println "ENABLED")
            (recur more [] (dissoc state :disabled?)))

           :else (recur more new-buf state)))

       :else (let [new-buf (if (or (contains? allowed-chars c)
                                   (digit? c))
                             (conj buf c)
                             [])]
              ;; (println (apply str new-buf))
               (recur more new-buf state))))))

(def sample2-memory "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(comment

  (parse-instructions (seq sample2-memory))

  (def part2-answer 85508223)
  (->> (:parsed (parse-instructions {:extended? true} (seq (raw-memory) #_sample-memory)))
       (compute-result))

  (def part1-answer 187825547)
  (->> (:parsed (parse-instructions (seq (raw-memory) #_sample-memory)))
       (compute-result))

  \n)
