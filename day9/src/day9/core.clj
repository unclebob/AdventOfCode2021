(ns day9.core
  (:require [clojure.set :as set]))

(defn- char-to-int [c]
  (let [i (int c)
        i (- i (int \0))]
    i))

(defn parse-line [line]
  (map char-to-int line))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn get-height [data [x y]]
  (let [row (nth data y 10)]
    (if (= 10 row)
      10
      (nth row x 10))))

(defn low? [data p]
  (let [[x y] p
        pn [x (dec y)]
        pe [(inc x) y]
        ps [x (inc y)]
        pw [(dec x) y]
        hn (get-height data pn)
        he (get-height data pe)
        hs (get-height data ps)
        hw (get-height data pw)
        h (get-height data p)]
    (and (< h hn)
         (< h hs)
         (< h he)
         (< h hw))))

(defn find-lows [data]
  (let [coords (for [y (range (count data))
                     x (range (count (first data)))]
                 [x y])]
    (set (filter (partial low? data) coords))))

(defn find-basin-points
  ([data basin-point]
   (find-basin-points data basin-point #{}))

  ([data basin-point basin-points]
   (let [[x y] basin-point
         height (get-height data basin-point)]
     (if (or (>= height 9)
             (contains? basin-points basin-point))
       basin-points
       (let [basin-points (conj basin-points basin-point)
             basin-points (set/union (find-basin-points data [(inc x) y] basin-points))
             basin-points (set/union (find-basin-points data [(dec x) y] basin-points))
             basin-points (set/union (find-basin-points data [x (inc y)] basin-points))
             basin-points (set/union (find-basin-points data [x (dec y)] basin-points))]
         basin-points))))
  )

(defn solve-1 [input]
  (let [input (parse-input input)
        lows (find-lows input)
        heights (map (partial get-height input) lows)
        heights (map inc heights)]
    (reduce + heights)))

(defn solve-2 [input]
  (let [data (parse-input input)
        lows (find-lows data)]
    (loop [lows lows basins []]
      (if (empty? lows)
        (reduce * (take-last 3 (sort (map count basins))))
        (let [basin-points (find-basin-points data (first lows))
              lows (set/difference lows basin-points)
              ]
          (recur lows (conj basins basin-points)))))))

