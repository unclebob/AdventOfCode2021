(ns day18.core
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(defn parse-line [input]
  (read-string input))

(defn- cs->s [cs]
  (apply str cs))

(defn get-token [s]
  (if (empty? s)
    nil
    (let [s (.trim s)
          num (re-find #"^\d+" s)]
      (if (nil? num)
        (first s)
        (Integer/parseInt (.trim num)))))
  )

(defn skip-token [s]
  (if (empty? s)
    ""
    (let [s (.trim s)
          num (re-find #"^\d+" s)]
      (if (nil? num)
        (.substring s 1)
        (.substring s (count num))))))

(defn complete-explosion [exploder left-num sf]
  (let [sf (cs->s sf)
        a (-> exploder skip-token get-token)
        b (-> exploder skip-token skip-token get-token)
        l (get-token left-num)
        l (if (nil? l) "" (+ a l))
        rs (re-find #"\d.*$" (-> exploder skip-token skip-token skip-token))
        r (get-token rs)
        r (if (nil? r) "" (+ r b))
        prefix (if (nil? left-num)
                 (.substring sf 0 (- (count sf) (count exploder)))
                 (.substring sf 0 (- (count sf) (count left-num))))
        first-interval (if (nil? left-num)
                         ""
                         (.substring
                           left-num
                           0
                           (- (count left-num) (count exploder))))
        first-interval (skip-token first-interval)
        second-interval (-> exploder skip-token skip-token skip-token skip-token)
        second-interval (if (nil? rs)
                          second-interval
                          (.substring
                            second-interval
                            0
                            (- (count second-interval)
                               (count rs))))
        rs (skip-token rs)
        ]
    (read-string (str prefix " " l " " first-interval " 0 " second-interval r rs))))

(defn explode [sf]
  (let [sf (pr-str sf)]
    (loop [cs sf
           level 0
           left-num nil
           exploder nil]
      (if (empty? cs)
        nil
        (let [token (get-token cs)]
          (cond
            (= token \[)
            (recur (skip-token cs)
                   (inc level)
                   left-num
                   (if (= level 4)
                     cs
                     exploder))

            (= token \])
            (recur (skip-token cs)
                   (dec level)
                   left-num
                   exploder)

            (and (number? token) (< level 5))
            (recur (skip-token cs)
                   level
                   cs
                   exploder)

            (and (number? token) (= level 5))
            (complete-explosion exploder left-num sf)

            :else
            (recur (skip-token cs)
                   level
                   left-num
                   exploder)))))))

(defn split-number [n]
  (let [h (quot n 2)]
    (if (= n (* h 2))
      [h h]
      [h (inc h)])))

(defn do-split [sf]
  (loop [sf (pr-str sf)
         ssf ""
         done false]
    (if (empty? sf)
      (read-string ssf)
      (let [token (get-token sf)]
        (if (and (number? token)
                 (>= token 10)
                 (not done))
          (recur (skip-token sf)
                 (str ssf " " (pr-str (split-number token)))
                 true)
          (recur (skip-token sf)
                 (str ssf " " token)
                 done))))))


(defn split [sf]
  (let [ssf (do-split sf)]
    (if (= ssf sf)
      nil
      ssf)))

(defn reduce-sf [sf]
  (let [xsf (explode sf)
        ssf (split sf)]
    (cond
      (and (nil? xsf) (nil? ssf))
      sf

      (some? xsf)
      (recur xsf)

      :else
      (recur ssf))))

(defn add-sf [a b]
  (let [sum-sf [a b]]
    (reduce-sf sum-sf))
  )

(defn magnitude [sf]
  (if (number? sf)
    sf
    (+ (* 3 (magnitude (first sf)))
       (* 2 (magnitude (second sf))))))

(defn solve-1 [input]
  (let [lines (string/split-lines input)
        sfs (map parse-line lines)]
    (magnitude (reduce add-sf sfs))))

(defn solve-2 [input]
  (let [lines (string/split-lines input)
        sfs (map parse-line lines)
        pairs (combo/combinations sfs 2)
        pairs (concat pairs (map reverse pairs))
        mags (map #(magnitude (apply add-sf %)) pairs)]
    (apply max mags)))