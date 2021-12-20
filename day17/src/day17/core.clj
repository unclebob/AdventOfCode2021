(ns day17.core)

(defn parse-input [input]
  (let [ns (re-seq #"[-]?\d+" input)]
    (map #(Integer/parseInt %) ns)))

(defn in? [[min-x max-x min-y max-y] [x y]]
  (and
    (>= x min-x)
    (<= x max-x)
    (>= y min-y)
    (<= y max-y)))

(defn beyond? [[min-x max-x min-y max-y] [x y]]
  (or
    (> x max-x)
    (< y min-y)))

(defn max-vx [[min-x max-x min-y max-y]]
  (inc max-x))

(defn min-v [dist]
  (let [d (Math/sqrt (+ 1 (* 8 dist)))
        v (/ (- d 1) 2)]
    (int v)))

(defn min-vx [[min-x max-x min-y max-y]]
  (min-v min-x))

(defn min-vy [[min-x max-x min-y max-y]]
  0)

(defn max-vy [[min-x max-x min-y max-y]]
  (Math/abs ^int (dec min-y)))

(defn shot [box [vx vy]]
  (loop [p [0 0]
         vx vx
         vy vy
         h 0]
    (cond
      (beyond? box p)
      :miss

      (in? box p)
      h

      :else
      (let [h (max h (second p))
            p [(+ vx (first p))
               (+ vy (second p))]
            vx (max 0 (dec vx))
            vy (dec vy)
            ]
        (recur p vx vy h)))))

(defn solve-1 [input]
  (let [box (parse-input input)
        vx-range (range (min-vx box) (inc (max-vx box)))
        vy-range (range (min-vy box) (inc (max-vy box)))
        heights (for [vx vx-range vy vy-range]
                  (shot box [vx vy]))
        heights (remove #(= :miss %) heights)]
    (apply max heights)))

(defn solve-2 [input]
  (let [box (parse-input input)
        vx-range (range (min-vx box) (inc (max-vx box)))
        max-vy (inc (max-vy box))
        vy-range (range (- max-vy) max-vy)
        heights (for [vx vx-range vy vy-range]
                  (shot box [vx vy]))
        heights (remove #(= :miss %) heights)]
    (count heights)))