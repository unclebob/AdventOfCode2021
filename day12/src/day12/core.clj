(ns day12.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-line [line]
  (re-seq #"\w+" line))

(defn parse-lines [lines]
  (map parse-line (string/split-lines lines)))

(defn add-path [path-map [a b]]
  (let [a-paths (get path-map a #{})
        b-paths (get path-map b #{})
        a-paths (if (= "start" b)
                  a-paths
                  (conj a-paths b))
        b-paths (if (= "start" a)
                  b-paths
                  (conj b-paths a))
        path-map (assoc path-map a a-paths)
        path-map (assoc path-map b b-paths)]
    path-map))

(defn paths->map [paths]
  (loop [paths paths
         path-map {}]
    (if (empty? paths)
      path-map
      (recur (rest paths)
             (add-path path-map (first paths))))))

(defn parse-input [input]
  (let [paths (parse-lines input)
        path-map (paths->map paths)]
    path-map))

(defn small? [cave]
  (re-matches #"[a-z]+" cave))

(defn format-path [path]
  (string/join "," path))

(defn format-paths [paths]
  (set (map format-path paths)))

(defn special-cave-visits [special-cave cur-path]
  (count (filter #(= special-cave %) cur-path)))

(defn find-paths
  ([path-map]
   (find-paths path-map nil))

  ([path-map special-cave]
   (let [end (path-map "end")
         start (path-map "start")]
     (if (or (nil? start) (nil? end))
       []
       (find-paths path-map special-cave "start" []))))

  ([path-map special-cave cave cur-path]
   (let [new-cur-path (conj cur-path cave)
         cur-path-set (set cur-path)
         skip-cave? (and (contains? cur-path-set cave)
                         (small? cave))
         special-case? (and
                         (= cave special-cave)
                         (< (special-cave-visits special-cave cur-path) 2))
         skip-cave? (if (nil? special-cave)
                      skip-cave?
                      (and skip-cave? (not special-case?)))]
     (cond
       (= "end" cave)
       [new-cur-path]

       skip-cave?
       []

       :else
       (loop [neighbors (path-map cave)
              paths []]
         (if (empty? neighbors)
           paths
           (let [p (find-paths path-map
                               special-cave
                               (first neighbors)
                               new-cur-path)]
             (recur (rest neighbors)
                    (concat paths p)))))))
   )
  )

(defn solve-2 [path-map]
  (let [small-caves (keys path-map)
        small-caves (remove #(= "start" %) small-caves)
        small-caves (remove #(= "end" %) small-caves)
        small-caves (filter small? small-caves)
        paths (set (mapcat #(find-paths path-map %) small-caves))]
    (count paths)))
