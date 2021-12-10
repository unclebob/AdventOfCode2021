(ns day8.core
  (:require [clojure.set :as set]))

(def segment-schema {0 #{\A \B \C \E \F \G}
                     1 #{\C \F}
                     2 #{\A \C \D \E \G}
                     3 #{\A \C \D \F \G}
                     4 #{\B \C \D \F}
                     5 #{\A \B \D \F \G}
                     6 #{\A \B \D \E \F \G}
                     7 #{\A \C \F}
                     8 #{\A \B \C \D \E \F \G}
                     9 #{\A \B \C \D \F \G}})

(def outputs-to-digit-map (set/map-invert segment-schema))


(defn parse-line [line]
  (let [tokens (re-seq #"[abcdefg]+" line)]
    [(vec (take 10 tokens)) (vec (drop 10 tokens))])
  )

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        lines (vec (remove empty? lines))]
    (vec (map parse-line lines))))

(defn- unique-count? [n]
  (or (= 2 n) (= 4 n) (= 3 n) (= 7 n)))

(defn count-unique [display]
  (let [counts (map count display)
        counts (filter unique-count? counts)]
    (count counts)))

(defn to-digit [c]
  (condp = c
    2 1
    4 4
    3 7
    7 8
    nil))

(defn propose-outputs-for-signals [displayed-digit]
  (let [digit (to-digit (count displayed-digit))]
    (if (nil? digit)
      nil
      (let [outputs (segment-schema digit)]
        (map #(vector % outputs) displayed-digit))))
  )

(defn- infer-outputs [proposals signal]
  (let [possible-outputs (get proposals signal)
        possible-outputs (map second possible-outputs)]
    [signal (reduce set/intersection possible-outputs)]))

(defn possible-outputs [all-displayed-digits]
  (let [proposals (mapcat propose-outputs-for-signals
                          all-displayed-digits)
        proposals (group-by first proposals)
        signals (keys proposals)
        signals-to-outputs (map #(infer-outputs proposals %)
                                signals)]
    (apply hash-map (flatten signals-to-outputs))))

(defn signals-for-obvious-digits [displayed-digits]
  (let [d4 (filter #(= 4 (count %)) displayed-digits)
        d1 (filter #(= 2 (count %)) displayed-digits)
        d7 (filter #(= 3 (count %)) displayed-digits)]
    {4 (set (map identity (first d4)))
     1 (set (map identity (first d1)))
     7 (set (map identity (first d7)))}))

(defn- to-set [signals]
  (set (map identity signals)))

(defn signals-for-length [n displayed-digits]
  (let [signals (filter #(= n (count %)) displayed-digits)
        signal-sets (map to-set signals)]
    (vec signal-sets)))

(defn map-signals-to-outputs [displayed-digits]
  (let [obvious-digits (signals-for-obvious-digits displayed-digits)
        d1 (obvious-digits 1)
        d7 (obvious-digits 7)
        d4 (obvious-digits 4)
        oA (first (set/difference d7 d1))
        d069 (signals-for-length 6 displayed-digits)
        d235 (signals-for-length 5 displayed-digits)
        d6 (first (filter #(not (set/subset? d7 %)) d069))
        oC (first (set/difference d7 d6))
        oF (first (set/difference d7 #{oA oC}))
        d9 (first (filter #(set/subset? d4 %) d069))
        d0 (first (remove #(or (= d9 %) (= d6 %)) d069))
        oG (first (set/difference d9 d4 d7))
        oD (reduce set/intersection d235)
        oD (first (set/difference oD d0))
        oB (first (set/difference d4 #{oC oD oF}))
        oE (first (set/difference d0 d9))
        ]
    {oA \A
     oC \C
     oF \F
     oG \G
     oD \D
     oB \B
     oE \E}))

(defn outputs-to-digits [signal-map signals]
  (let [output-set (set (map signal-map signals))]
    (outputs-to-digit-map output-set)))

(defn translate-display [[digits displays]]
  (let [signal-map (map-signals-to-outputs digits)
        digits (map #(outputs-to-digits signal-map %) displays)]
    (reduce #(+ (* 10 %1) %2) digits)))

(defn solve-1 [input]
  (let [input (parse-input input)
        displays (map second input)
        uniques (map count-unique displays)]
    (reduce + uniques)))

(defn solve-2 [input]
  (let [input (parse-input input)
        displays (map translate-display input)]
    (reduce + displays)))