(ns day19.core
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo])
  (:use flatland.ordered.set))

(defn parse-line [s]
  (let [ns (re-seq #"-?\d+" s)]
    (condp = (count ns)
      3
      (map #(Integer/parseInt %) ns)

      1
      (Integer/parseInt (first ns))

      nil)))

(defn parse-input [input]
  (loop [lines (string/split-lines input)
         reports {}
         scanner nil]
    (if (empty? lines)
      reports
      (let [line (parse-line (first lines))]
        (cond
          (number? line)
          (recur (rest lines)
                 (assoc reports line [])
                 line)

          (nil? line)
          (recur (rest lines) reports scanner)

          :else
          (recur (rest lines)
                 (update reports scanner conj line)
                 scanner))))))

(defn rotate-axis [[x y]]
  [[x y] [y (- x)] [(- x) (- y)] [(- y) x]])

(defn rotations-x [[x y z]]
  (let [plane [y z]
        rots (rotate-axis plane)]
    (map #(vector x (first %) (second %)) rots)))

(defn rotations-y [[x y z]]
  (let [plane [x z]
        rots (rotate-axis plane)]
    (map #(vector (first %) y (second %)) rots)))

(defn rotations-z [[x y z]]
  (let [plane [x y]
        rots (rotate-axis plane)]
    (map #(vector (first %) (second %) z) rots)))

(defn orientations [p]
  (apply ordered-set
         (concat
           (rotations-x p)
           (rotations-y p)
           (rotations-z p)
           (mapcat rotations-y (rotations-x p))
           (mapcat rotations-z (rotations-x p)))))

(defn scanner-orientations [scanner]
  (let [point-orientations (map orientations scanner)]
    (apply map list point-orientations)))

(defn translate-point [[dx dy dz] [x y z]]
  [(+ x dx) (+ y dy) (+ z dz)])

(defn translate-scanner [offset scanner]
  (map #(translate-point offset %) scanner))

(defn coincident? [scanner point]
  (cond
    (empty? scanner)
    false

    (= point (first scanner))
    true

    :else
    (recur (rest scanner) point)))

(defn coincidences [s1 s2]
  (loop [s1 s1
         s2 s2
         hits #{}]
    (if (empty? s1)
      hits
      (recur (rest s1)
             s2
             (if (coincident? s2 (first s1))
               (conj hits (first s1))
               hits)))))

(defn offset [[bx by bz] [x y z]]
  [(- bx x) (- by y) (- bz z)])

(defn all-offsets [beacons scanner]
  (set
    (for [beacon beacons
          point scanner]
      (offset beacon point))))

(defn find-best-translation [beacons scanner hit-limit]
  (loop [offsets (all-offsets beacons scanner)
         best-offset nil
         best-hits 0
         best-translation nil]
    (if (empty? offsets)
      (if (nil? best-offset)
        nil
        [best-translation best-offset])
      (let [this-offset (first offsets)
            translation (translate-scanner this-offset scanner)
            this-hits (count (coincidences beacons translation))]
        (cond
          (< this-hits hit-limit)
          (recur (rest offsets)
                 best-offset
                 best-hits
                 best-translation)

          (>= this-hits hit-limit)
          [translation this-offset]

          :else
          (recur (rest offsets)
                 best-offset
                 best-hits
                 best-translation))))))

(defn- best-match [matches]
  (loop [matches matches
         best-n 0
         best-match nil]
    (if (empty? matches)
      best-match
      (let [this-match (first matches)
            [points offset] this-match
            this-n (count points)]
        (if (> this-n best-n)
          (recur (rest matches) this-n this-match)
          (recur (rest matches) best-n best-match))))))

(defn find-best-orientation [beacons scanner hits]
  (let [so (scanner-orientations scanner)
        matches (map #(find-best-translation beacons % hits) so)
        matches (remove nil? matches)]
    (best-match matches)))

(defn solve-1 [report]
  (loop [beacons (set (report 0))
         scanners (vec (vals (dissoc report 0)))]
    (if (empty? scanners)
      beacons
      (let [this-scanner  (first scanners)
            best (find-best-orientation beacons this-scanner 12)
            [points offset] best
            _ (prn offset)]
        (if (nil? best)
          (recur beacons (conj (vec (rest scanners)) this-scanner))
          (recur (set/union beacons points) (rest scanners)))))))

(defn manhattan [[[x1 y1 z1] [x2 y2 z2]]]
  (+
    (Math/abs ^int (- x1 x2))
    (Math/abs ^int (- y1 y2))
    (Math/abs ^int (- z1 z2))
    ))

(defn solve-2 [offsets]
  (let [pairs (combo/combinations offsets 2)
        dists (map manhattan pairs)]
    (apply max dists)))