(ns day15.core-spec
  (:require [speclj.core :refer :all]
            [day15.core :refer :all]))

(describe "parsing"
  (it "creates risk map"
    (should= {[0 0] 1
              [1 0] 2
              [2 0] 3
              [0 1] 4
              [1 1] 5
              [2 1] 6}
             (parse-input "123\n456\n"))))

(describe "utilities"
  (it "knows neighbors"
    (should= #{[5 4]
               [4 5] [6 5]
               [5 6]}
             (all-neighbors [5 5] 10 10))

    (should= #{[1 0] [0 1]}
             (all-neighbors [0 0] 10 10))

    (should= #{[5 5]
               [4 6]}
             (all-neighbors [5 6] 5 6)))

  (context "diagonal"
    (it "1x1"
      (should= [[0 0]]
               (diagonals 0 0)))
    (it "2x2"
      (should= [[1 1] [0 1] [1 0] [0 0]]
               (diagonals 1 1)))

    (it "3x3"
      (should= [[2 2] [1 2] [2 1] [0 2] [1 1] [2 0] [0 1] [1 0] [0 0]]
               (diagonals 2 2))))

  (context "cost-map"
    (it "2x2"
      (let [risk-map (parse-input "12\n34")]
        (should= {[0 0] 7
                  [1 0] 6
                  [0 1] 7
                  [1 1] 4}
                 (cost-map risk-map 1 1)))))

  (context "expand input"
    (it "does one right expansion of 1x1"
      (should= {[1 0] 2}
               (right-expansion {[0 0] 1})))

    (it "does one right expansion of 2x2"
      (should= {[2 0] 2 [3 0] 3
                [2 1] 7 [3 1] 1}
               (right-expansion {[0 0] 1 [1 0] 2
                                 [0 1] 6 [1 1] 9})))

    (it "expands 1x1 right"
      (should= {[0 0] 1 [1 0] 2}
               (expand-right-by 1 {[0 0] 1})))

    (it "expands 2x2 right by 2"
          (should= {[0 0] 1 [1 0] 2 [2 0] 2 [3 0] 3 [4 0] 3 [5 0] 4
                    [0 1] 4 [1 1] 8 [2 1] 5 [3 1] 9 [4 1] 6 [5 1] 1}
                   (expand-right-by 2 {[0 0] 1 [1 0] 2
                                       [0 1] 4 [1 1] 8})))

    (it "expands 1x2 down by 2"
      (should= {[0 0] 6 [1 0] 8
                [0 1] 7 [1 1] 9
                [0 2] 8 [1 2] 1}
               (expand-down-by 2 {[0 0] 6 [1 0] 8})))

    (it "expands 1x1"
      (should= {[0 0] 7 [1 0] 8
                [0 1] 8 [1 1] 9}
               (expand-by 1 {[0 0] 7})))

    (it "expands all"
      (let [sample (parse-input (slurp "sample"))
            samplex5 (parse-input (slurp "samplex5"))]
        (should= samplex5 (expand-by 4 sample))))
    )
  )

(describe "solutions"
  (context "part 1"
    (it "solves sample"
      (should= 40 (solve-1 (slurp "sample"))))

    (it "solves problem"
      (should= 390 (solve-1 (slurp "input"))))

    )
  (context "part 2"
    (it "solves samplex5"
      (should= 315 (solve-1 (slurp "samplex5"))))

    (it "solves sample"
      (should= 315 (solve-2 (slurp "sample"))))

    (it "solves problem"
      (should= 2814 (solve-2 (slurp "input"))))
 )
  )


