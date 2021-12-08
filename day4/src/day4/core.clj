(ns day4.core
  (:require [clojure.string :as s]))

(defn parse-input [input]
  (let [lines (s/split-lines input)
        lines (remove #(= "" %) lines)
        draws (map #(Integer/parseInt %) (s/split (first lines) #","))
        boards (map
                 (fn [line] (map
                              #(Integer/parseInt %)
                              (remove empty? (s/split line #"\s+"))))
                 (rest lines))
        boards (doall (partition 5 boards))]
    [draws boards]))

(defn play-board [draw board]
  (map (fn [row] (map #(if (= draw %) "X" %) row)) board))

(defn play [draw boards]
  (map #(play-board draw %) boards))



(defn row-win? [row] (= row ["X" "X" "X" "X" "X"]))

(defn win? [board]
  (or (some row-win? board)
      (some row-win? (apply map list board))))


(defn score [draw board]
  (if (win? board)
    (* draw (reduce + (remove #(= "X" %) (flatten board))))
    -1))

(defn bingo-winner [draws boards]
  (loop [draws draws boards boards n 0]
    (if (empty? draws)
      nil
      (let [n (inc n)
            draw (first draws)
            boards (play draw boards)
            scores (map #(score draw %) boards)
            top-score (apply max scores)
            ]
        (if (neg? top-score)
          (recur (rest draws) boards n)
          top-score)
        ))))

(defn bingo-loser [draws boards]
  (loop [draws draws boards boards n 0 last-scores nil last-n nil]
    (if (empty? draws)
      [last-scores n]
      (let [n (inc n)
            draw (first draws)
            boards (play draw boards)
            winners (filter win? boards)
            scores (map #(score draw %) winners)
            boards (remove win? boards)
            ]
        (if (empty? scores)
          (recur (rest draws) boards n last-scores last-n)
          (recur (rest draws) boards n scores n))
        ))))

(defn bingo [draws boards]
  (loop [draws draws boards boards n 0 wins [] board-map {}]
    (if (empty? draws)
      board-map
      (let [n (inc n)
            draw (first draws)
            boards (play draw boards)
            winners (filter win? boards)
            _ (prn "---" n)
            _ (prn winners)
            _ (prn (map #(score draw %) winners))
            scores (map #(score draw %) boards)
            boards (remove win? boards)
            ]
        (recur (rest draws) boards n (conj wins [scores draw n])
               (assoc board-map n boards))
        ))))