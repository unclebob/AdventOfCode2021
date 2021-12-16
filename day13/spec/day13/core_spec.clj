(ns day13.core-spec
  (:require [speclj.core :refer :all]
            [day13.core :refer :all]))

(describe "parsing"
  (it "parses a line"
    (should= [[] []] (parse-line ""))
    (should= [[5 6] []] (parse-line "5,6"))
    (should= [[] [0 7]] (parse-line "fold along y=7"))
    (should= [[] [5 0]] (parse-line "fold along x=5")))

  (it "parses many lines"
    (should= {:coords #{[1 2] [3 4]}
              :folds [[0 7] [5 0]]}
             (parse-input "1,2\n3,4\nfold along y=7\nfold along x=5\n")))
  )

(describe "utilities"
  (it "folds"
    (should= #{} (fold #{} [0 0]))
    (should= #{[1 1]} (fold #{[1 1]} [0 1]))
    (should= #{[1 0] [2 1] [2 2]}
             (fold #{[1 0] [2 1] [2 4] [2 5]} [0 3]))

    (should= #{[0 0] [1 1] [0 2]}
             (fold #{[0 0] [1 1] [3 1] [4 2]}
                   [2 0]))
    )
  )

(describe "solutions"
  (context "part 1"
    (it "should solve sample"
      (should= 17 (solve-1 (slurp "sample"))))

    (it "should solve problem"
      (should= 763 (solve-1 (slurp "input")))))

  (context "part 2"
    (it "folds the problem"
      (let [answer (solve-2 (slurp "input"))]
        (draw answer)
        )))
  )
