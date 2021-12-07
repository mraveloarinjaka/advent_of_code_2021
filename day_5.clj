(ns com.mrave.advent-of-code-2021.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2


")

(defn parse-line
  [input]
  (when-let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" input)]
    [[(Integer/parseInt x1) (Integer/parseInt y1)] [(Integer/parseInt x2) (Integer/parseInt y2)]]))

(defn expand-line
  [[[x1 y1] [x2 y2] :as line]]
  (cond
    (= x1 x2) #_> (for [y (range (min y1 y2) (+ 1 (max y1 y2)))] [x1 y])
    (= y1 y2) #_> (for [x (range (min x1 x2) (+ 1 (max x1 x2)))] [x y1])
    :else (let [vx (- x2 x1)
                dx (if (< 0 vx) 1 -1)
                vy (- y2 y1)
                dy (if (< 0 vy) 1 -1)
                new-starting-point [(+ x1 dx) (+ y1 dy)]]
            (conj (expand-line [new-starting-point [x2 y2]]) [x1 y1]))))

#_ (expand-line [[9 4] [3 4]])
#_ (expand-line [[9 7] [7 9]])
#_ (expand-line [[9 7] [5 9]])

(defn parse-input
  [input-to-parse]
  (->> input-to-parse
       str/split-lines
       (map parse-line)
       (map expand-line)
       set ;;remove duplicated lines
       (mapcat identity)))

#_ (parse-input sample-input)

(defn compute-canvas-index
  [width [x y]]
  (+ x (* y width)))

(defn compute-canvas
  ([width points]
   (loop [[current-point & remaining-points] points
          canvas {}]
     (let [index (compute-canvas-index width current-point)
           new-canvas (update canvas index #(if % (inc %) 1))]
     (if (seq remaining-points)
       (recur remaining-points new-canvas)
       new-canvas))))
  ([points]
   (let [[minX maxX]  (apply (juxt min max) (map first points))
         width (inc maxX)
         [minY maxY] (apply (juxt min max) (map second points))
         height (inc maxY)
         canvas (compute-canvas width points)]
     {:origin [minX minY] :height height :width width :canvas canvas})))

#_ (compute-canvas (parse-input sample-input))

(defn render-cell
  [[x y] width canvas]
  (let [cell (get canvas (compute-canvas-index width [x y]))]
    (if cell
      (format "%3d" cell)
      "   ")))

(defn render-line
  [y width canvas]
  (str "["
       (apply str (for [x (range width)]
                    (render-cell [x y] width canvas)))
       "]"))

(defn render-canvas
  [height width canvas]
  (for [y (range height)]
    (render-line y width canvas)))

(defn draw
  [points]
  (let [{:keys [height width canvas]} (compute-canvas points)]
    (doseq [line (render-canvas height width canvas)]
      (prn line))))

(->> sample-input
     parse-input
     draw)

(defn compute-overlap
  [canvas]
  (->> canvas
       (map second)
       (filter (partial < 1))
       count))

(let [{:keys [height width canvas]} (->> sample-input
                                         parse-input
                                         compute-canvas)]
  (doseq [line (render-canvas height width canvas)]
    (prn line))
  (compute-overlap canvas))

(let [{:keys [height width canvas]} (->> (slurp "resources\\day_5_input.txt")
                                         parse-input
                                         compute-canvas)]
  (compute-overlap canvas))

