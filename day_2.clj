(ns com.mrave.advent-of-code-2021.day-2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (line-seq (io/reader "resources\\day_2_input.txt")))

(defn move
  [position {:keys [direction variation] :as movement}]
  (case direction
    "up" (update position :depth #(- % variation))
    "down" (update position :depth #(+ % variation))
    "forward" (update position :horizontal-position #(+ % variation))))

(defn parse-instructions
  [instructions]
  (->> instructions
    (map #(str/split % #" "))
    (map (fn [[direction variation]]
           {:direction direction :variation (Integer. variation)}))))

(defn apply-instructions
  [move-fn initial-position instructions]
  (reduce (fn [position movement] (move-fn position movement)) initial-position instructions))

(def sample-input
"forward 5
down 5
forward 8
up 3
down 8
forward 2")

(->> sample-input
     str/split-lines
     parse-instructions
     (apply-instructions move {:horizontal-position 0 :depth 0}))

(->> input
     parse-instructions
     (apply-instructions move {:horizontal-position 0 :depth 0}))

(defn move-according-to-aim
  [{:keys [aim] :as position}
   {:keys [direction variation] :as movement}]
  (case direction
    "up" (update position :aim #(- % variation))
    "down" (update position :aim #(+ % variation))
    "forward" (-> position
                  (update :horizontal-position #(+ % variation))
                  (update :depth #(+ % (* aim variation))))))

(->> sample-input
     str/split-lines
     parse-instructions
     (apply-instructions move-according-to-aim {:horizontal-position 0 :depth 0 :aim 0}))

(->> input
     parse-instructions
     (apply-instructions move-according-to-aim {:horizontal-position 0 :depth 0 :aim 0}))
