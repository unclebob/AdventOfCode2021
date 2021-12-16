(ns day13.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-line [line]
  (cond
    (re-matches #"\d+,\d+" line)
    (let [coords (re-seq #"\d+" line)
          coords (map #(Integer/parseInt %) coords)]
      [coords []])

    (re-matches #"fold along y=\d+", line)
    (let [y (first (re-seq #"\d+" line))
          y (Integer/parseInt y)]
      [[] [0 y]])

    (re-matches #"fold along x=\d+", line)
    (let [x (first (re-seq #"\d+" line))
          x (Integer/parseInt x)]
      [[] [x 0]])

    :else
    [[] []]))

(defn parse-input [input]
  (loop [lines (string/split-lines input)
         result {:coords #{} :folds []}]
    (if (empty? lines)
      result
      (let [[coord fold] (parse-line (first lines))
            result (if (not-empty coord)
                     (update result :coords conj coord)
                     result)
            result (if (not-empty fold)
                     (update result :folds conj fold)
                     result)]
        (recur (rest lines) result)))))

(defn fold [coords [x y]]
  (cond
    (zero? x)
    (let [sides (group-by #(> (second %) y) coords)
          folded (set (map
                        (fn [[cx cy]]
                          [cx (- y (- cy y))]) (sides true)))]
      (set/union folded (set (sides false))))

    (zero? y)
    (let [sides (group-by #(> (first %) x) coords)
          folded (set (map
                        (fn [[cx cy]]
                          [(- x (- cx x)) cy]) (sides true)))]
      (set/union folded (set (sides false))))

    :else
    :error))

(defn solve-1 [input]
  (let [{:keys [coords folds]} (parse-input input)
        coords (fold coords (first folds))]
    (count coords)))

(defn solve-2 [input]
  (let [{:keys [coords folds]} (parse-input input)]
    (loop [coords coords
           folds folds]
      (if (empty? folds)
        coords
        (recur (fold coords (first folds)) (rest folds))))
    ))

(defn draw [coords]
  (let [max-x (apply max (map first coords))
        max-y (apply max (map second coords))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (print (if (contains? coords [x y]) "*" " ")))
      (println))))