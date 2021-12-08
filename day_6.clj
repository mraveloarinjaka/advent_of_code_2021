(ns com.mrave.advent-of-code-2021.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input "3,4,3,1,2")
(def input-day-6 (first (str/split-lines (slurp "resources\\day_6_input.txt"))))


(defn parse-input
  [input-to-parse]
  (->> (str/split input-to-parse #",")
       (map (fn [internal-timer] (array-map :internal-timer (Integer/parseInt internal-timer))))))

(def DEFAULT-TIMER 8)
(def RESET-TIMER 6)

(defn after-one-day
  [{:keys [internal-timer]}]
  (if (zero? internal-timer)
    [{:internal-timer RESET-TIMER} {:internal-timer DEFAULT-TIMER}]
    [{:internal-timer (dec internal-timer)}]))

(defn population-after-n-days
  [starting-population n]
  (loop [population starting-population days n]
    (if (zero? days)
      population
      (recur (->> population
                  (pmap after-one-day)
                  (apply concat))
             (dec days)))))

#_ (time (-> sample-input
             parse-input
             (population-after-n-days 80)
             count))

(def after-one-day-memoized (memoize after-one-day))

(defn population-evolution
  [starting-population]
  (cons starting-population (lazy-seq (population-evolution (mapcat after-one-day-memoized starting-population))))) 

#_ (time (-> sample-input
             parse-input
             population-evolution
             (nth 80)
             count))

;;;;;;;;;;;;;;;;;;;;;;
;; this never ended ;;
;;;;;;;;;;;;;;;;;;;;;;

#_ (-> input-day-6
       parse-input
       (population-after-n-days 256)
       count)

;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defn initial-population
  [starting-population]
  (let [empty-population (zipmap (range (inc DEFAULT-TIMER)) (repeat 0))]
    (reduce (fn [population internal-timer]
              (update population internal-timer inc))
            empty-population
            starting-population)))

(defn after-one-day-bis
  [population]
  (reduce (fn [new-population internal-timer]
            (let [count-at-that-age (get population internal-timer)]
              (cond
                (zero? count-at-that-age) new-population
                (zero? internal-timer) (-> new-population
                                           (update 0 (partial + (* -1 count-at-that-age)))
                                           (update DEFAULT-TIMER (partial + count-at-that-age))
                                           (update RESET-TIMER (partial + count-at-that-age)))
                :else (-> new-population
                          (update (dec internal-timer) (partial + count-at-that-age))
                          (update internal-timer (partial + (* -1 count-at-that-age)))))))
          population (range (inc DEFAULT-TIMER))))

(defn evolution-population-bis
  [population iterations]
  (reduce (fn [new-population _]
            (after-one-day-bis new-population))
          population
          (range iterations)))

(defn count-population
  [population]
  (->> population
       (map second)
       (reduce +)))

#_ (let [starting-population 
         (->> sample-input
              parse-input
              (map :internal-timer)
              initial-population)]
     (count-population (evolution-population-bis starting-population 256)))

#_ (let [starting-population 
         (->> input-day-6
              parse-input
              (map :internal-timer)
              initial-population)]
     (count-population (evolution-population-bis starting-population 256)))

