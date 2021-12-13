(ns com.mrave.advent-of-code-2021.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def one-sample-input
  "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(def sample-input
"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn parse-input
  [intput-to-parse]
  (->> intput-to-parse
       str/split-lines
       (map #(str/split % #" \| "))
       (map #(let [[signal-pattern output-value] %] {:signal-pattern signal-pattern :output-value output-value}))))

(def DIGITS-WITH-UNIQUE-NUMBER-OF-SEGMENTS
  {2 "1" 
   4 "4"
   3 "7"
   7 "8"})

(defn count-digits-with-unique-number-of-segments
  [output-value]
  (->> output-value
       (#(str/split % #" "))
       (map count)
       (filter #(get DIGITS-WITH-UNIQUE-NUMBER-OF-SEGMENTS %))
       count))

(reduce + (->> sample-input
               parse-input
               (map :output-value)
               (map count-digits-with-unique-number-of-segments)
               ))

(reduce + (->> (slurp "resources\\day_8_input.txt")
               parse-input
               (map :output-value)
               (map count-digits-with-unique-number-of-segments)
               ))

(def DIGITS-WITH-SAME-NUMBER-OF-SEGMENTS
  {6 ["0" "6" "9"]
   5 ["5" "2" "3"]})

(defn extract-1-4-7-8
  [signal-pattern]
  (into {}  (for [[number digit] DIGITS-WITH-UNIQUE-NUMBER-OF-SEGMENTS]
    [(Integer/parseInt digit) (first (filter #(= number (count %)) signal-pattern))])))

(defn extract-0-6-9
  [signal-pattern]
  (filter #(= 6 (count %)) signal-pattern))

(defn extract-6
  "6 is the only digit that does not contain 7 among 0,6 and 9"
  [pattern-7 signal-pattern]
  (let [pattern-0-6-9 (extract-0-6-9 signal-pattern)
        pattern-7-set (set pattern-7)]
    (first (filter #(not= pattern-7-set (clojure.set/intersection pattern-7-set (set %))) pattern-0-6-9))))

(defn extract-9
  "6 is the only digit that contains 4 among 0,6 and 9"
  [pattern-4 signal-pattern]
  (let [pattern-0-6-9 (extract-0-6-9 signal-pattern)
        pattern-4-set (set pattern-4)]
    (first (filter #(= pattern-4-set (clojure.set/intersection pattern-4-set (set %))) pattern-0-6-9))))

(defn extract-0
  "0 is known once we have 6 and 9"
  [pattern-6 pattern-9 signal-pattern]
  (let [pattern-0-6-9 (extract-0-6-9 signal-pattern)]
    (first (remove #{pattern-6 pattern-9} pattern-0-6-9))))

(defn extract-5-2-3
  [signal-pattern]
  (filter #(= 5 (count %)) signal-pattern))

(defn extract-3
  "3 is the only digit that contains 1 among 5,2 and 3"
  [pattern-1 signal-pattern]
  (let [pattern-5-2-3 (extract-5-2-3 signal-pattern)
        pattern-1-set (set pattern-1)]
    (first (filter #(= pattern-1-set (clojure.set/intersection pattern-1-set (set %))) pattern-5-2-3))))

(defn extract-5
  "3 is the only digit contained in 6 among 5,2 and 3"
  [pattern-6 signal-pattern]
  (let [pattern-5-2-3 (extract-5-2-3 signal-pattern)
        pattern-6-set (set pattern-6)]
    (first (filter #(let [pattern-set (set %)]
                      (= pattern-set (clojure.set/intersection pattern-6-set pattern-set))) pattern-5-2-3))))

(defn extract-2
  "2 is known once we have 3 and 5"
  [pattern-3 pattern-5 signal-pattern]
  (let [pattern-5-2-3 (extract-5-2-3 signal-pattern)]
    (first (remove #{pattern-3 pattern-5} pattern-5-2-3))))

(defn extract-digits
  [signal-pattern-to-parse]
  (let [signal-pattern (str/split signal-pattern-to-parse #" ")
        pattern-1-4-7-8 (extract-1-4-7-8 signal-pattern)
        pattern-6 (extract-6 (get pattern-1-4-7-8 7) signal-pattern)
        pattern-9 (extract-9 (get pattern-1-4-7-8 4) signal-pattern)
        pattern-0 (extract-0 pattern-6 pattern-9 signal-pattern)
        pattern-3 (extract-3 (get pattern-1-4-7-8 1) signal-pattern)
        pattern-5 (extract-5 pattern-6 signal-pattern)
        pattern-2 (extract-2 pattern-3 pattern-5 signal-pattern)]
    #_(prn {:pattern-1-4-7-8 pattern-1-4-7-8
          :pattern-6 pattern-6
          :pattern-9 pattern-9
          :pattern-0 pattern-0
          :pattern-3 pattern-3
          :pattern-5 pattern-5
          :pattern-2 pattern-2})
    {(set (get pattern-1-4-7-8 8)) 8
     (set pattern-5) 5
     (set pattern-2) 2
     (set pattern-3) 3
     (set (get pattern-1-4-7-8 7)) 7
     (set pattern-9) 9
     (set pattern-6) 6
     (set (get pattern-1-4-7-8 4)) 4
     (set pattern-0) 0
     (set (get pattern-1-4-7-8 1)) 1}))

#_(let [signal-pattern (->> one-sample-input
                          parse-input
                          first
                          :signal-pattern)]
   (extract-digits signal-pattern))

(defn decode-output
  [{:keys [signal-pattern output-value]}]
  (let [digits (extract-digits signal-pattern)]
    (->> output-value
         (#(str/split % #" "))
         (map #(get digits (set %)))
         (apply str))))

(->> one-sample-input
     parse-input
     first
     decode-output)

(->> sample-input
     parse-input
     (map decode-output)
     (map #(Integer/parseInt %))
     (reduce +)
     )

(->> (slurp "resources\\day_8_input.txt")
     parse-input
     (map decode-output)
     (map #(Integer/parseInt %))
     (reduce +))
