(ns day11.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))


(defn- char-to-num [char]
  (let [i (int char)]
    (- i (int \0))))

(defn parse-line [line]
  (map char-to-num line))

(defn parse-lines [lines]
  (map parse-line (string/split-lines lines)))

(defn grid-to-map [grid]
  (let [index-map (map-indexed
                    (fn [v row]
                      (map-indexed (fn [h cell]
                                     [[h v] cell]) row))
                    grid)]
    (loop [index-map (apply concat index-map)
           grid-map {}]
      (if (empty? index-map)
        grid-map
        (recur (rest index-map)
               (assoc grid-map (first (first index-map))
                               (second (first index-map))))))))
(defn parse-input [input]
  (grid-to-map (parse-lines input)))

(defn- out-of-bounds [world [x y]]
  (let [limit (int (Math/sqrt (count world)))]
    (or (>= x limit) (>= y limit) (neg? x) (neg? y)))
  )

(defn get-diagonals [world [x y :as p]]
  (let [points (for [cx (range (dec x) (+ x 2))
                     cy (range (dec y) (+ y 2))]
                 [cx cy])
        points (remove #(= % p) points)
        points (remove #(out-of-bounds world %) points)]
    (set points)))

(defn increment-energy
  ([world]
   (increment-energy world (keys world)))

  ([world coords]
   (loop [coords coords
          world world]
     (if (empty? coords)
       world
       (recur (rest coords)
              (update world (first coords) inc)))))
  )

(defn increment-diagonals [world diagonals]
  (if (empty? diagonals)
    world
    (recur (increment-energy world (first diagonals))
           (rest diagonals))))

(defn clear-flashers [world flashers]
  (if (empty? flashers)
    world
    (recur (assoc world (first flashers) 0)
           (rest flashers))))

(defn flash
  ([world]
   (flash world #{}))

  ([world flashers]
   (let [new-flashers (filter (fn [[_ v]]
                                (>= v 10)) world)
         new-flashers (set (map first new-flashers))
         new-flashers (set/difference new-flashers flashers)
         all-flashers (set/union new-flashers flashers)]
     (if (empty? new-flashers)
       [(count all-flashers) (clear-flashers world all-flashers)]
       (let [diagonals (map #(get-diagonals world %) new-flashers)
             world (increment-diagonals world diagonals)]
         (recur world all-flashers))))))

(defn solve-1 [world]
  (loop [n 100 flashes 0 world world]
    (if (zero? n)
      flashes
      (let [n (dec n)
            world (increment-energy world)
            [new-flashes world] (flash world)]
        (recur n (+ flashes new-flashes) world)))))

(defn solve-2 [world]
  (loop [n 1 world world]
    (let [world (increment-energy world)
          [flashes world] (flash world)]
      (if (= 100 flashes)
        n
        (recur (inc n) world)))))