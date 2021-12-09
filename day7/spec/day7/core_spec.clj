(ns day7.core-spec
  (:require [speclj.core :refer :all]
            [day7.core :refer :all]))


(describe "parse input"
  (it "parses the input"
    (should= [1 2 3] (parse-input "1,2,3\n"))))

(describe "utilities"
  (it "should calculate mean"
    (should= 2 (mean [1 2 3])))

  (it "should find distances from a point"
    (should= [1 0 1] (distances 2 [1 2 3])))

  (it "should find the sum of distances from a point"
    (should= 2 (sum-distances 2 [1 2 3])))

  (it "should calculate cost of movement for part 2"
    (should= 5050 (cost-2 100)))

  )

(def sample "16,1,2,0,4,2,7,1,2,14")

(describe "part 1"
  (it "should solve sample"
    (should= 37 (solve-1 sample)))

  (it "should solve problem"
    (should= 359648 (solve-1 (slurp "input"))))
  )

(describe "part 2"
  (it "should solve sample"
    (should= 168 (solve-2 sample)))

  (it "should solve prolem"
    (should= 100727924 (solve-2 (slurp "input")))))
