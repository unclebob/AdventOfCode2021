(ns day7.core)

(defn parse-input [input]
  (let [input (.trim input)
        input (clojure.string/split input #",")]
    (map #(Integer/parseInt %) input)))

(defn mean [ns]
  (/ (reduce + ns) (count ns)))

(defn- abs [n]
  (if (neg? n) (- n) n))

(defn distances [p ns]
  (map #(abs (- p %)) ns))

(defn sum-distances [p ns]
  (reduce + (distances p ns)))

(defn cost-2 [n]
  (/ (+ n (* n n)) 2))

(defn sum-cost-2 [p ns]
  (reduce + (map cost-2 (distances p ns))))

(defn solve-1 [input]
  (let [input (parse-input input)
        start (apply min input)
        stop (apply max input)
        trials (map #(sum-distances % input)
                    (range start (inc stop)))]
    (apply min trials)))

(defn solve-2 [input]
  (let [input (parse-input input)
        start (apply min input)
        stop (apply max input)
        trials (map #(sum-cost-2 % input)
                    (range start (inc stop)))]
    (apply min trials)))
