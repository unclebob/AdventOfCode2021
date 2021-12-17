(ns day15.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn mc-indexed [fn l]
  (apply concat
         (map-indexed fn l)))

(defn parse-input [input]
  (let [lines (string/split-lines input)]
    (apply hash-map
           (mc-indexed
             (fn [y line]
               (mc-indexed
                 (fn [x risk]
                   [[x y] (Integer/parseInt (str risk))])
                 line))
             lines))))

(defn all-neighbors [[x y] max-x max-y]
  (let [ns [[(inc x) y] [x (inc y)]
            [(dec x) y][x (dec y)]
            ]
        ns (remove #(or (neg? (first %))
                        (neg? (second %))) ns)
        ns (remove #(or (> (first %) max-x)
                        (> (second %) max-y)) ns)]
    (set ns)))

(defn right-down-neighbors [[x y] max-x max-y]
  (let [ns [[(inc x) y] [x (inc y)]
            ]
        ns (remove #(or (neg? (first %))
                        (neg? (second %))) ns)
        ns (remove #(or (> (first %) max-x)
                        (> (second %) max-y)) ns)]
    (set ns)))

(defn- a-diagonal [x y]
  (loop [dx x
         dy y
         diag []]
    (if (= dx y)
      (conj diag [dx dy])
      (recur (inc dx) (dec dy) (conj diag [dx dy])))))

(defn diagonals [x y]
  (let [x-starters (range x -1 -1)
        x-starters (concat x-starters (repeat y 0))
        y-starters (range y -1 -1)
        y-starters (concat (repeat x y) y-starters)]
    (mapcat a-diagonal x-starters y-starters)))

(def infinity 1e50)

(defn risk
  ([risk-map cost-map max-x max-y coord level]
   (risk risk-map cost-map max-x max-y coord 0 #{} infinity level))

  ([risk-map cost-map max-x max-y coord risk-so-far visited lowest level]
   (let [this-risk (get risk-map coord)
         risk-so-far (+ risk-so-far this-risk)
         visited (conj visited coord)
         ns (if (> level 1)
              (right-down-neighbors coord max-x max-y)
              (all-neighbors coord max-x max-y))]
     (cond
       (> risk-so-far lowest)
       infinity

       (= coord [max-x max-y])
       (min risk-so-far lowest)

       :else
       (let [ns (set/difference ns visited)]
         (if (empty? ns)
           infinity
           (loop [ns ns
                  lowest lowest]
             (if (empty? ns)
               lowest
               (let [neighbor (first ns)
                     this (if (contains? cost-map neighbor)
                            (+ risk-so-far (cost-map neighbor))
                            (risk risk-map
                                  cost-map
                                  max-x
                                  max-y
                                  neighbor
                                  risk-so-far
                                  visited
                                  lowest
                                  (inc level)))]
                 (recur (rest ns) (min this lowest)))))))))))

(defn cost-map
  ([risk-map max-x max-y]
   (cost-map risk-map max-x max-y {} (diagonals max-x max-y)))

  ([risk-map max-x max-y costs coords]
   (loop [coords coords
          costs costs]
     (if (empty? coords)
       costs
       (let [coord (first coords)
             cost (risk risk-map costs max-x max-y coord 0)
             costs (assoc costs coord cost)]
         (recur (rest coords) costs))))))

(defn- inc-val [val]
  (if (= 9 val)
    1
    (inc val)))

(defn- ex-r [width [[x y] v]]
  [[(+ x width) y] (inc-val v)])

(defn- ex-d [height [[x y] v]]
  [[x (+ y height)] (inc-val v)])

(defn right-expansion [risk-map]
  (let [coords (keys risk-map)
        xs (map first coords)
        width (inc (- (apply max xs) (apply min xs)))
        pairs (mapcat (partial ex-r width) risk-map)]
    (apply hash-map pairs))
  )

(defn down-expansion [risk-map]
  (let [coords (keys risk-map)
        ys (map second coords)
        height (inc (- (apply max ys) (apply min ys)))
        pairs (mapcat (partial ex-d height) risk-map)]
    (apply hash-map pairs))
  )

(defn expand-right-by [n risk-map]
  (let [expansions (take n (rest (iterate right-expansion risk-map)))
        expansions (apply merge expansions)]
    (apply merge risk-map expansions))
  )

(defn expand-down-by [n risk-map]
  (let [expansions (take n (rest (iterate down-expansion risk-map)))
        expansions (apply merge expansions)]
    (apply merge risk-map expansions))
  )

(defn expand-by [n risk-map]
  (let [risk-map (expand-right-by n risk-map)
        risk-map (expand-down-by n risk-map)]
    risk-map
    ))

(defn solve-1 [input]
  (let [risk-map (parse-input input)
        lines (string/split-lines input)
        max-y (dec (count lines))
        max-x (dec (count (first lines)))
        costs (cost-map risk-map max-x max-y)]
    (- (costs [0 0]) (risk-map [0 0]))))

(defn solve-2 [input]
  (let [risk-map (parse-input input)
        risk-map (expand-by 4 risk-map)
        coords (keys risk-map)
        max-x (apply max (map first coords))
        max-y (apply max (map second coords))
        costs (cost-map risk-map max-x max-y)]
    (- (costs [0 0]) (risk-map [0 0]))))


