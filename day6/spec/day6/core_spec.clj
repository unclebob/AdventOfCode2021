(ns day6.core-spec
  (:require [speclj.core :refer :all]
            [day6.core :refer :all]))

(describe "can parse input"
  (it "can parse input"
    (should= [0 1 1 1 0 0 0 0 0 0] (parse-input "1,2,3"))))

(describe "growth of population"
  (it "counts ages down"
    (should= [3 5 7 1 3 6 2 5 1 0] (age-fish [1 3 5 7 1 3 6 2 5 1])))

  (it "counts births"
    (should= 1 (count-births [1 2 0 4 0 5])))

  (it "reproduces"
    (should= [3 9 7 4 2 1 7 9 3 12] (reproduce [3 9 7 4 2 1 7 6 3 9])))

  (it "does a day"
    (should= [5 7 6 8 2 4 6 7 10 0] (day [1 5 7 6 8 2 4 5 7 9])))
  )


(def sample "3,4,3,1,2")

(describe "part 1"
  (it "should solve sample"
    (should= 5934 (solve-1 sample 80)))

  (it "should solve problem"
    (should= 359999 (solve-1 (.trim (slurp "input")) 80)))
  )

(describe "part 2"
  (it "should solve sample"
    (should= 26984457539 (solve-1 sample 256)))

  (it "should solve problem"
    (should= 1631647919273 (solve-1 (.trim (slurp "input")) 256))))
