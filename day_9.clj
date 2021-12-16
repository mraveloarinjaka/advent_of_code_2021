(ns com.mrave.advent-of-code-2021.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
"2199943210
3987894921
9856789892
8767896789
9899965678")

(defn parse-input
  [input-to-parse]
  (let [input (->> input-to-parse
       str/split-lines
       (map seq))]
    {:width (count (first input))
     :length (count input)
     :heightmap 
     (apply concat (for [row input]
       (map #(Integer/parseInt (str %)) row)))}))

(defn index->coord
  [& {:keys [width index]}]
  {:y (quot index width)
   :x (mod index width)})

(defn coord->index
  [& {:keys [width x y]}]
  (+ (* width y) x))

(index->coord :width 2 :index 7)
(coord->index :x 1 :y 3 :width 2)

(defn adjacents
  [width length [x y]]
  (let [potentials [[(- x 1) y]
                    [x (- y 1)]
                    [(+ 1 x) y]
                    [x (+ 1 y)]]]
    (remove (fn [[px py]] (or (< px 0) (< py 0) (<= width px) (<= length py))) potentials)))

(defn ->input
  [{:keys [width length heightmap] :as parsed-input}]
  {:width width
   :length length
   :heightmap
   (->> (map (fn [index height]
               (let [{:keys [x y]} (index->coord :index index :width width)]
                 [[x y]
                  {:neighbors (adjacents width length [x y]) :height height}])) (range (count heightmap)) heightmap)
        (apply concat)
        (apply hash-map))})

(defn lowest-neighboring-point
  [heightmap neighbors]
  ;(prn heightmap)
  (apply min (map #(:height (get heightmap %)) neighbors))
  ;(map #(:height (get heightmap %)) neighbors)
  )

(defn find-neighbors-lowest
  [heightmap [[x y] info]]
  (let [{:keys [neighbors height]} info
          lowest (lowest-neighboring-point heightmap neighbors)]
    [[x y] height lowest]))

(defn lowest?
  [[[x y] height lowest]]
  (< height lowest))

(defn ->risk-level
  [[[x y] height lowest]]
  (inc height))

(let [heightmap (->> sample-input
                     ;(slurp "resources\\day_9_input.txt")
                     parse-input
                     ->input
                     :heightmap)
      xf (comp (map (partial find-neighbors-lowest heightmap))
               (filter lowest?)
               (map ->risk-level))]
  (transduce xf + 0 heightmap))

(let [input (->> sample-input
                     ;(slurp "resources\\day_9_input.txt")
                     parse-input
                     ->input)
      heightmap (->> input
                     :heightmap)
      xf-lowests (comp (map (partial find-neighbors-lowest heightmap))
                      (filter lowest?)
                      )]
  (->> {:input input
        :lowests (transduce xf-lowests conj heightmap)}
       :lowests))
