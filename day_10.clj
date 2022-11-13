(ns com.mrave.advent-of-code-2021.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]") 

(def opening-delimiters {"(" ")"
                       "{" "}"
                         "[" "]"
                         "<" ">"})

(def closing-delimiters
(into {} (for [[opening closing] opening-delimiters] [closing opening])))

#_(opening-delimiters "[")

(defn extract-chunks
    [line-to-process]
    (loop [line line-to-process
           opened-chunks '()
           valid-chunks []
           status :valid]
      (let [[token & remaining] line]
        (if token
          (cond
            (opening-delimiters token)
            #_> (recur remaining (conj opened-chunks token) valid-chunks :valid)
            (closing-delimiters token)
            #_> (let [opening (closing-delimiters token)]
                  (if (= opening (first opened-chunks))
                    (recur remaining (rest opened-chunks) (conj valid-chunks [opening token]) :valid)
                    (recur nil opened-chunks valid-chunks {:corrupted token})))
            :else (recur nil opened-chunks valid-chunks {:corrupted token}))
          {:opened-chunks opened-chunks
           :valid-chunks valid-chunks
           :status status
           }))))

(def corrupted-score {")" 3 "]" 57 "}" 1197 ">" 25137})

(def AUTO-COMPLETE-SCORE {")" 1 "]" 2 "}" 3 ">" 4})

(defn compute-score
 [global-score character]
(+ (* global-score 5) (get AUTO-COMPLETE-SCORE character)))

#_(reduce compute-score 0 (map str "])}>")) ; 294

(comment

  ;; compute auto complete score
  (->> #_sample-input
      (slurp "resources/day_10_input.txt")
      str/split-lines
      (map #(map str %))
      (map extract-chunks)
      (remove #(get-in % [:status :corrupted]))
      ;(map :status)
      (map :opened-chunks)
      (map #(map opening-delimiters %))
      (map #(reduce compute-score 0 %))
      sort
      vec
      ((fn [coll]
         (let [n (count coll)
               middle-index (max 0 (quot n 2))]
           (get coll middle-index))))
      ) ; 1605968119

  ;;compute corrupted score
  (->> sample-input
      #_(slurp "resources/day_10_input.txt")
      str/split-lines
      (map #(map str %))
      (map extract-chunks)
      (map :status)
      (map :corrupted)
      (remove nil?)
      (map corrupted-score)
      (reduce +))

  )
