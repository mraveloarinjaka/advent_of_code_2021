(ns com.mrave.advent-of-code-2021.day-11
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def sample-energy-levels
"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(defn parse
  [input]
  (->> input
       (str/split-lines)
       (map #(map str %))
       (apply concat)
       (map #(Integer. %))
       vec
       ))

(def WIDTH 10)

(defn ->grid
  [input & {:keys [width] :or {width WIDTH}}] 
  (let [len (count input)]
    {:width width
     :height (quot len width)
     :grid (into {} (for [index (range len)]
                      [[(mod index width)
                        (quot index width)] 
                       (get input index)]))}))

#_(->grid (parse sample-energy-levels))

(defn ->index
  [[x y] & {:keys [width] :or {width WIDTH}}]
  (+ (* y width) x))

(defn print-grid
  [{:keys [width height grid]}]
  (let [result (transient (vec (range (* width height))))]
    (->> (reduce-kv (fn [res k v]
                      (assoc! res (->index k) (str v)))
                    result
                    grid)
         (persistent!)
         (partition width)
         (map #(apply str %))
         (reduce (fn [res row] (str res row "\n")) ""))))

#_(-> (parse sample-energy-levels)
      ->grid
      print-grid
      )

(defn neighbors
  [[x y] & {:keys [width height]
            :or {width WIDTH height WIDTH}}]
  (set (for [dx [-1 0 1] dy [-1 0 1]
        :let [nx (+ x dx)
              ny (+ y dy)]
        :when (and (not= [0 0] [dx dy])
                   (<= 0 nx)
                   (< nx width)
                   (<= 0 ny)
                   (< ny height))]
    [nx ny])))

#_ (neighbors [9 9])

(defn increase-energy-levels
  [grid & {:keys [only] :or {only (set (keys grid))}}]
  (reduce (fn [res k] (update res k inc)) grid only))

(def FLASH-LEVEL 9)

(defn extract-next-to-flash
  [grid]
  (set (for [[k v] grid
             :when (< FLASH-LEVEL v)]
         k)))

(defn reset-energy-levels
  [grid already-flashed]
  (persistent! (reduce (fn [res k] (assoc! res k 0))
                       (transient grid)
                       already-flashed)))

(defn flash
  [grid next-to-flash & {:keys [width height] :or {width WIDTH height WIDTH}}]
  (let [to-increase-after-a-flash (reduce (fn [res neighbor]
                                            (apply conj res (neighbors neighbor)))
                                          []
                                          next-to-flash)]
    (increase-energy-levels grid :only to-increase-after-a-flash)))

(defn flash!
  [input]
  (loop [result input already-flashed #{}]
    (let [{:keys [grid]} result
          next-to-flash (set/difference (extract-next-to-flash grid)
                                         already-flashed)]
      (println "next-to-flash" next-to-flash)
      (if (empty? next-to-flash)
        (update result :grid reset-energy-levels already-flashed)
        (let [grid-after-a-flash (flash grid next-to-flash (select-keys result [:width :height]))]
          (recur (assoc result :grid grid-after-a-flash)
                 (set/union already-flashed next-to-flash)))))))

#_(-> "6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637" 
      #_sample-energy-levels
      parse
      ->grid
      (update :grid increase-energy-levels)
      flash!
      print-grid
      println
      )


(defn execute-one-step
  [input]
  (-> input
      (update :grid increase-energy-levels)
      flash!))

(comment 

  (let [step-0 (atom (-> #_sample-energy-levels
                         (slurp "resources/day_11_input.txt")
                         parse
                         (->grid)
                         ))
        nb-of-flashes (atom 0)
        total-nb-of-flashes (atom 0)
        synchronized-flashes (atom [])]
    (dotimes [n 2000]
      (println "step" (inc n))
      (println "before")
      (println (print-grid (deref step-0)))
      (swap! step-0 execute-one-step)
      (reset! nb-of-flashes (count (for [[_ v] (:grid (deref step-0)) :when (= 0 v)] v)))
      (when (= (deref nb-of-flashes) 100)
        (swap! synchronized-flashes conj (inc n)))
      (swap! total-nb-of-flashes + (deref nb-of-flashes))
      (println "after" (deref total-nb-of-flashes) "flashes")
      (println (print-grid (deref step-0)))
      )
    (println (deref synchronized-flashes)))

  )


