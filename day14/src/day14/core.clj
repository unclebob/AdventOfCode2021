(ns day14.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))


(defn parse-line [line]
  (if (re-matches #"\w\w -> \w" line)
    [:insert [(.substring line 0 2)
              (.substring line 6 7)]]
    [:template line])
  )

(defn parse-input [input]
  (loop [lines (remove empty? (string/split-lines input))
         result {:template "" :insert {}}]
    (if (empty? lines)
      result
      (let [[type data] (parse-line (first lines))]
        (if (= type :template)
          (recur (rest lines)
                 (assoc result :template data))
          (recur (rest lines)
                 (assoc-in result [:insert (first data)] (second data))))))))

(defn polymerize [insertions [head & tail]]
  (loop [result (str head)
         head head
         tail tail]
    (if (empty? tail)
      result
      (let [dimer (str head (first tail))
            insertion (insertions dimer)
            result (if (nil? insertion)
                     (str result (last dimer))
                     (str result insertion (last dimer)))]
        (recur result (str (first tail)) (rest tail))))))

(defn count-dimers [template]
  (loop [template template
         counts {}]
    (if (< (count template) 2)
      counts
      (let [dimer (apply str (take 2 template))
            count (get counts dimer 0)]
        (recur (rest template)
               (assoc counts dimer (inc count)))))))

(defn count-insertions [insertion-map dimer-counts]
  (loop [dimers (keys dimer-counts)
         new-counts {}]
    (if (empty? dimers)
      new-counts
      (let [dimer (first dimers)
            count (get dimer-counts dimer 0)
            insertion (get insertion-map dimer)]
        (if (nil? insertion)
          (recur (rest dimers) (assoc new-counts dimer count))
          (let [dimer1 (str (first dimer) insertion)
                dimer2 (str insertion (second dimer))
                count1 (get new-counts dimer1 0)
                count2 (get new-counts dimer2 0)
                new-counts (assoc new-counts
                             dimer1 (+ count1 count)
                             dimer2 (+ count2 count))]
            (recur (rest dimers) new-counts)))))))

(defn solve-1 [input]
  (let [{:keys [template insert]} (parse-input input)
        iterations (iterate (partial polymerize insert) template)
        tenth (nth iterations 10)
        counts (vals (frequencies tenth))
        ]
    (- (apply max counts) (apply min counts))))

(defn count-elements [dimer-counts]
  (loop [dimer-counts dimer-counts
         element-counts {}]
    (if (empty? dimer-counts)
      element-counts
      (let [[dimer count] (first dimer-counts)
            element (first dimer)
            element-count (get element-counts element 0)
            element-count (+ element-count count)]
        (recur (rest dimer-counts)
               (assoc element-counts element element-count))))))

(defn solve-2 [n input]
  (let [{:keys [template insert]} (parse-input input)
        initial-counts (count-dimers template)
        iterations (iterate (partial count-insertions insert) initial-counts)
        dimer-counts (nth iterations n)
        element-counts (count-elements dimer-counts)
        element-counts (update element-counts (last template) inc)
        counts (vals element-counts)
        ]
    (- (apply max counts) (apply min counts))))