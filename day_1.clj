(ns com.mrave.advent-of-code-2021.day-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (line-seq (io/reader "resources\\day_1_input.txt")))

(defn count-increases
  [inputed-measurements]
  (loop [[current-measurement & remaining-measurements] inputed-measurements
         previous-measurement nil
         number-of-increases (atom 0)]
    (when (and previous-measurement
               (> (Integer. current-measurement) (Integer. previous-measurement)))
      (swap! number-of-increases inc))
    (if (seq remaining-measurements)
      (recur remaining-measurements current-measurement number-of-increases)
      @number-of-increases)))

(defn count-increases-by-3
  [inputed-measurements]
  (if (<= (count inputed-measurements) 3)
    0
    (loop [measurements (drop 3 inputed-measurements)
           current-window (vec (take 3 inputed-measurements))
           previous-window []
           number-of-increases (atom 0)]
      (when (= (count previous-window) 3)
        (let [current-sum (apply + (map #(Integer. %) current-window))
              previous-sum (apply + (map #(Integer. %) previous-window))]
          (when (> current-sum previous-sum)
            (swap! number-of-increases inc))))
      (if-not (seq measurements)
        @number-of-increases
        (let [[current-measurement & remaining-measurements] measurements]
          (let [new-current-window (conj (vec (drop 1 current-window)) current-measurement)
                new-previous-window current-window]
            (recur remaining-measurements
                   new-current-window
                   new-previous-window
                   number-of-increases)))))))

(count-increases [199 200 208 210 200 207 240 269 260 263])
(count-increases-by-3 [199 200 208 210 200 207 240 269 260 263])

(count-increases input)
;1688

(count-increases-by-3 input)
;1728
