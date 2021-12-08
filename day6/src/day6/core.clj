(ns day6.core)

(defn parse-input [input]
  (let [fish-list (map #(Integer/parseInt %)
                    (clojure.string/split input #","))
        ]
    (loop [school []
           age 0]
      (if (= 10 age)
        school
        (let [fish-of-age (count (filter #(= age %) fish-list))]
          (recur (conj school fish-of-age)
                 (inc age)))))
    ))

(defn age-fish [school]
  (conj (vec (rest school)) 0))

(defn count-births [school]
  (first school))

(defn reproduce [school]
  (let [births (count-births school)
        [a b c d e f g h i j] school]
    [a b c d e f g (+ births h) i (+ births j)]))

(defn day [school]
  (-> school reproduce age-fish))

(defn solve-1 [input days]
  (let [school (parse-input input)
        school (nth (iterate day school) days)]
    (reduce + school)))