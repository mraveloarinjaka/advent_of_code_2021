(ns com.mrave.advent-of-code-2021.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")

(defn parse-input
  [input-to-parse]
  (let [parsed-input (clojure.string/split-lines input-to-parse)
        [parsed-draws & parsed-grids] parsed-input
        draws (str/split parsed-draws #",")
        grids (->> parsed-grids
                   (partition-by #(= "" %))
                   (remove #(= '("") %))
                   (map (fn [grid]
                          (->> grid
                               (map #(str/split % #" "))
                               (mapcat #(remove (partial = "") %))))))]
    {:draws draws :grids grids}))

(defn apply-one-draw
  [grid draw]
  (mapv #(if (= draw %) "x" %) grid))

(def NUMBERS-PER-ROW 5)

(defn winning-column?
  [grid]
  (let [marked-indexes (->> grid
                            (map-indexed vector)
                            (filter #(= "x" (second %)))
                            (map (fn [[index _]] (mod index NUMBERS-PER-ROW))))]
    (->> (frequencies marked-indexes)
         (map second)
         set
         (#(% NUMBERS-PER-ROW))
         some?)))

(defn winning-row?
  [grid]
  (->> grid
       (partition NUMBERS-PER-ROW)
       (some (fn [row] (every? #(= "x" %) row)))))

(defn winning?
  [grid]
  (or (winning-row? grid) (winning-column? grid)))

(defn sum-unmarked
  [grid]
  (->> grid
       (map #(if (= "x" %) 0 (Integer/parseInt %)))
       (reduce +)))

(defn score
  [grid current-draw]
  (* (Integer/parseInt current-draw)
     (sum-unmarked grid)))

(defn compute-score
  [input-to-parse]
  (let [{:keys [draws grids]} (parse-input input-to-parse)]
    (loop [[current-draw & remaining-draws :as current-draws] draws
           current-grids grids
           scores []]
      (if (and (seq current-draws) (< 0 (count current-grids)))
        (let [new-grids (mapv #(apply-one-draw % current-draw) current-grids)
              [winning-grid & _ :as winning-grids] (filter winning? new-grids)
              remaining-grids (vec (remove winning? new-grids))
              _ (prn "current-draw: " current-draw)
              _ (prn "winning-grid: " winning-grid)
              _ (prn "score: " (score winning-grid current-draw))
              ]
          (if winning-grid
            (recur remaining-draws remaining-grids (conj scores (score winning-grid current-draw))) 
            (recur remaining-draws new-grids scores)))
        scores))))


(last (compute-score (slurp "resources\\day_4_input.txt")))

(comment

(parse-input sample-input)

{:draws ["7" "4" "9" "5" "11" "17" "23" "2" "0" "14" "21" "24" "10" "16" "13" "6" "15" "25" "12" "22" "18" "20" "8" "19" "3" "26" "1"], :grids (("22" "13" "17" "11" "0" "8" "2" "23" "4" "24" "21" "9" "14" "16" "7" "6" "10" "3" "18" "5" "1" "12" "20" "15" "19") ("3" "15" "0" "2" "22" "9" "18" "13" "17" "5" "19" "8" "7" "25" "23" "20" "11" "10" "24" "4" "14" "21" "16" "12" "6") ("14" "21" "17" "24" "4" "10" "16" "15" "9" "19" "18" "8" "23" "26" "20" "22" "11" "13" "6" "5" "2" "0" "12" "3" "7"))}

(def winning-grid '("14" "21" "17" "24" "4" "10" "16" "15" "9" "19" "18" "8" "23" "26" "20" "22" "11" "13" "6" "5" "2" "0" "12" "3" "7"))

(def winning-column-draws ["21" "16" "8" "11" "0"])
(def winning-row-draws ["7" "4" "9" "5" "11" "17" "23" "2" "0" "14" "21" "24"])

(def winning-column-grid (reduce apply-one-draw winning-grid winning-column-draws))
(def winning-row-grid (reduce apply-one-draw winning-grid winning-row-draws))

(->> winning-column-grid
     winning?)

(->> winning-row-grid
     winning?)

(sum-unmarked winning-grid)

)

