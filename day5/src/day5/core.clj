(ns day5.core)

(defn parse-line [s]
  (let [coords (re-seq #"\d+" s)
        coords (map #(Integer/parseInt %) coords)]
    (partition 2 coords)))

(defn parse-lines [s]
  (let [lines (clojure.string/split-lines s)]
    (map parse-line lines)))

(defn- unit [n]
  (cond (zero? n) 0
        (pos? n) 1
        :else -1))

(defn unit-vector [[[x1 y1] [x2 y2]]]
  [(unit (- x2 x1)) (unit (- y2 y1))]
  )

(defn add-vector [[vx vy] [x y]]
  [(+ x vx) (+ y vy)])

(defn points-on-line [line-segment]
  (let [[p1 p2] line-segment
        uv (unit-vector line-segment)]
    (loop [p1 p1 p2 p2 points #{}]
      (let [points (conj points p1)]
        (if (= p1 p2)
          points
          (recur (add-vector uv p1) p2 points))))))

(defn- accumulate-point [accumulator point]
  (let [point (vec point)
        c (get accumulator point 0)]
    (assoc accumulator point (inc c))))

(defn- accumulate-point-set [accumulator
                             point-set]
  (if (empty? point-set)
    accumulator
    (recur (accumulate-point
             accumulator
             (first point-set))
           (rest point-set))))

(defn accumulate-points [point-sets]
  (loop [point-sets point-sets
         accumulator {}]
    (if (empty? point-sets)
      accumulator
      (recur (rest point-sets)
             (accumulate-point-set
               accumulator
               (first point-sets))))))

(defn- orthogonal? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2))
  )

(defn solve-1 [input]
  (let [segments (parse-lines input)
        segments (filter orthogonal? segments)
        points (map points-on-line segments)
        accumulator (accumulate-points points)
        counts (vals accumulator)]
    (count (filter #(> % 1) counts))))

(defn solve-2 [input]
  (let [segments (parse-lines input)
        points (map points-on-line segments)
        accumulator (accumulate-points points)
        counts (vals accumulator)]
    (count (filter #(> % 1) counts))))
