(ns day10.core)

(defn parse-input [input]
  (clojure.string/split-lines input))

(defn closer-for [c]
  (condp = c
    \( \)
    \[ \]
    \{ \}
    \< \>
    nil))

(defn opener? [c]
  (or
    (= \( c)
    (= \{ c)
    (= \[ c)
    (= \< c)))

(defn corrupt [c]
  (condp = c
    \) :bad-p
    \] :bad-b
    \} :bad-c
    \> :bad-a))


(defn parse-chunk [s]
  (loop [s s
         c (list)]
    (cond
      (empty? s)
      (apply str c)

      (opener? (first s))
      (recur (rest s) (conj c (closer-for (first s))))

      (= (first s) (first c))
      (recur (rest s) (rest c))

      :else
      (corrupt (first s)))))

(defn score [c]
  (condp = c
    :bad-p 3
    :bad-b 57
    :bad-c 1197
    :bad-a 25137
    0))

(defn score-2 [c]
  (condp = c
    \) 1
    \] 2
    \} 3
    \> 4)
  )

(defn- score-completion [c]
  (let [scores (map score-2 c)]
    (reduce #(+ (* 5 %1) %2) scores)))

(defn solve-1 [input]
  (let [input (parse-input input)
        results (map parse-chunk input)
        scores (map score results)]
    (reduce + scores)))

(defn- incomplete? [s]
  (and
    (not (keyword? s))
    (not (empty? s))))

(defn solve-2 [input]
  (let [input (parse-input input)
        completions (filter incomplete? (map parse-chunk input))
        scores (map score-completion completions)
        n-scores (count scores)]
    (nth (sort scores) (quot n-scores 2))))