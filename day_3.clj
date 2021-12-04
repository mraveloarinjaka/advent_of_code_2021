(ns com.mrave.advent-of-code-2021.day-3
  (:require [clojure.java.io :as io]))

(defn gamma
  [[zeroes ones]]
  (if (< zeroes ones)
    \0
    \1))

(defn epsilon
  [[zeroes ones]]
  (if (< zeroes ones)
    \1
    \0))

(defn compute-frequencies
  [diagnostic-report]
  (->> diagnostic-report
       (map seq)
       (apply map vector) ;transpose
       (map frequencies)
       (map (fn [digit-frequencies]
              (let [zeroes (get digit-frequencies \0 0)
                    ones (get digit-frequencies \1 0)]
                [zeroes ones])))))

(defn extract-criteria
  [diagnostic-report criteria-fns]
  (->> diagnostic-report
       compute-frequencies
       ((apply juxt (for [criteria-fn criteria-fns] #(map criteria-fn %))))
       (map #(apply str %))
       ))

(defn compute-rates
  [diagnostic-report]
  (->> (extract-criteria diagnostic-report [gamma epsilon])
       (map #(Integer/parseInt % 2))))

(def input (line-seq (io/reader "resources\\day_3_input.txt")))

(def sample-input
"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(->> sample-input
     clojure.string/split-lines
     compute-rates
     (apply *))

(->> input
     compute-rates
     (apply *))

(defn oxygen-criteria
  [[zeroes ones]]
  (cond
    (< zeroes ones) \1
    (< ones zeroes) \0
    :else \1))

(defn co2-criteria
  [[zeroes ones]]
  (cond
    (< ones zeroes) \1
    (< zeroes ones) \0
    :else \0))

(defn compute-rating
  [diagnostic-report criteria-fn]
  (loop [readings diagnostic-report index 0]
    (let [[rating-criteria] (extract-criteria readings [criteria-fn])
          [reading & remaining :as readings-matching-criteria]
          (filter (fn [reading]
                    (= (get reading index) (get rating-criteria index)))
                  readings)]
      (if (seq remaining)
        (recur readings-matching-criteria (inc index))
        reading))))

(let [parsed-input (clojure.string/split-lines sample-input)
      oxygen-rating (compute-rating parsed-input oxygen-criteria)
      co2-rating (compute-rating parsed-input co2-criteria)]
  (prn {:oxygen-rating oxygen-rating :co2-rating co2-rating})
  (* (Integer/parseInt oxygen-rating 2) (Integer/parseInt co2-rating 2)))

(let [parsed-input input
      oxygen-rating (compute-rating parsed-input oxygen-criteria)
      co2-rating (compute-rating parsed-input co2-criteria)]
  (prn {:oxygen-rating oxygen-rating :co2-rating co2-rating})
  (* (Integer/parseInt oxygen-rating 2) (Integer/parseInt co2-rating 2)))
