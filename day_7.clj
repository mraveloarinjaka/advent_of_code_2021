(ns com.mrave.advent-of-code-2021.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input "16,1,2,0,4,2,7,1,2,14")

(defn parse-input
  [input-to-parse]
  (let [parsed-input (->> input-to-parse
                          str/split-lines
                          first
                          (#(str/split % #","))
                          (map #(Integer/parseInt %)))
        [min-horizontal-position max-horizontal-position] (apply (juxt min max) parsed-input)]
    {:min-horizontal-position min-horizontal-position
     :max-horizontal-position max-horizontal-position
     :starting-positions parsed-input}))

(defn distance
  [starting-position end-position]
  (java.lang.Math/abs (- end-position starting-position)))

(defn fuel-cost
  [distance-to-move]
  (/ (* distance-to-move (+ distance-to-move 1)) 2))

(defn cost-to-move
  ([fuel-cost-fn starting-positions end-position]
   (reduce + (map #(fuel-cost-fn (distance % end-position)) starting-positions)))
  ([starting-positions end-position]
   (cost-to-move fuel-cost starting-positions end-position)))

(defn cheapest-position-to-move-to
  ([fuel-cost-fn starting-positions potential-end-positions]
   (loop [[position & remaining-positions :as positions] potential-end-positions
          cheapest-position nil
          cheapest-cost nil]
     (if (seq positions)
       (let [cost (cost-to-move fuel-cost-fn starting-positions position)
             cheaper? (or (nil? cheapest-cost) (< cost cheapest-cost))
             new-cost (if cheaper? cost cheapest-cost)
             new-position (if cheaper? position cheapest-position)]
         (recur remaining-positions new-position new-cost))
       {:cheapest-position cheapest-position :cheapest-cost cheapest-cost})))
  ([starting-positions potential-end-positions]
   (cheapest-position-to-move-to fuel-cost starting-positions potential-end-positions)))

(let [{:keys [min-horizontal-position
              max-horizontal-position
              starting-positions]} (-> sample-input
                                       parse-input)]
  (cheapest-position-to-move-to identity starting-positions (range min-horizontal-position (inc max-horizontal-position))))

(let [{:keys [min-horizontal-position
              max-horizontal-position
              starting-positions]} (->> (slurp "resources\\day_7_input.txt")
                                        parse-input)]
  (cheapest-position-to-move-to starting-positions (range min-horizontal-position (inc max-horizontal-position))))
