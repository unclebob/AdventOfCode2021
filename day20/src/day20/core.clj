(ns day20.core
  (:require [clojure.string :as string]))

(defn parse-line [y line]
  (loop [line line
         image {}
         x 0]
    (if (empty? line)
      image
      (recur (rest line)
             (assoc image [x y] (first line))
             (inc x)))))

(defn parse-image [lines]
  (loop [lines lines
         image {}
         y 0]
    (if (empty? lines)
      image
      (recur (rest lines)
             (merge image (parse-line y (first lines)))
             (inc y)))))

(defn bounds [image]
  (let [coords (keys image)
        xs (map first coords)
        ys (map second coords)
        max-x (apply max xs)
        min-x (apply min xs)
        max-y (apply max ys)
        min-y (apply min ys)]
    [[min-x min-y] [max-x max-y]]))

(defn neighbors-of [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y] [x y] [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn binary-index-of
  ([image coord]
   (binary-index-of image coord \.))

  ([image coord default]
   (let [bits (neighbors-of coord)
         bits (map #(if (= \# (get image % default)) "1" "0") bits)
         bits (apply str bits)]
     (Integer/parseInt bits 2))))

(defn expand-box [[[x1 y1] [x2 y2]]]
  [[(- x1 1) (- y1 1)] [(+ x2 1) (+ y2 1)]])

(defn get-pixel [image enhancer point]
  (let [index (binary-index-of image point)
        pixel (nth enhancer index)]
    pixel))


(defn enhance [enhancer image]
  (let [box (bounds image)
        [[x1 y1] [x2 y2]] box
        new-image (for [x (range x1 (inc x2))
                        y (range y1 (inc y2))]
                    [[x y] (get-pixel image enhancer [x y])])
        new-image (reduce #(assoc %1 (first %2) (second %2)) {} new-image)]
    new-image))

(defn draw-image [image]
  (let [[[x1 y1] [x2 y2]] (bounds image)]
    (doseq [y (range y1 (inc y2))]
      (doseq [x (range x1 (inc x2))]
        (print (image [x y])))
      (println))))

(defn solve [input n]
  (let [lines (string/split-lines input)
        lines (remove empty? lines)
        enhancer (first lines)
        image (parse-image (rest lines))
        boxes (iterate expand-box (bounds image))
        nth-box (nth boxes n)
        inf-box (nth boxes (* 2 n))
        image (assoc image (first inf-box) \. (last inf-box) \.)
        images (iterate #(enhance enhancer %) image)
        final (nth images n)
        [[min-x min-y] [max-x max-y]] nth-box
        pixels (for [x (range min-x (inc max-x))
                     y (range min-y (inc max-y))]
                 (final [x y]))]
    (count (filter #(= \# %) pixels))))
