(ns day18.core-spec
  (:require [speclj.core :refer :all]
            [day18.core :refer :all]))


(describe "parsing"
  (it "parses"
    (should= [[1 2] [3 4]]
             (parse-line "[[1,2],[3,4]]"))))

(describe "utiltities"
  (context "get-token"
    (it "finds numbers"
      (should= 27 (get-token " 27 ")))
    (it "finds brackets"
      (should= \[ (get-token " [27"))
      (should= \] (get-token " ] [27")))
    )

  (context "skip-token"
    (it "skips numbers"
      (should= "]" (skip-token " 27]"))
      (should= "[27 24]" (skip-token "][27 24]"))
      ))

  (context "explode"
    (it "will not explode"
      (should= nil (explode [1 2]))
      (should= nil (explode [[1 2] 0]))
      (should= nil (explode [[[1 2] 0] 0]))
      (should= nil (explode [[[[1 2] 0] 0] 0])))

    (it "will explode case 1 no left number"
      (should= [[[[0 36] 0] 0] 0] (explode [[[[[1 27] 9] 0] 0] 0]))
      )

    (it "will explode case 2 left and right number"
      (should= [[[10 [0 28]] 0] 0] (explode [[[9 [[1 27] 1]] 0] 0])))

    (it "will explode case 3 no right number"
      (should= [0 [0 [0 [10 0]]]] (explode [0 [0 [0 [9 [1 27]]]]])))

    (it "does samples"
      (should= [[[[0, 9], 2], 3], 4]
               (explode [[[[[9, 8], 1], 2], 3], 4]))
      (should= [7, [6, [5, [7, 0]]]]
               (explode [7, [6, [5, [4, [3, 2]]]]]))
      (should= [[6, [5, [7, 0]]], 3]
               (explode [[6, [5, [4, [3, 2]]]], 1]))
      (should= [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]]
               (explode [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]]))
      (should= [[3, [2, [8, 0]]], [9, [5, [7, 0]]]]
               (explode [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]]))
      )
    )

  (context "split"
    (it "won't split"
      (should= nil (split [0 0]))
      (should= nil (split [0 [1 [2 [9 9]]]])))

    (it "will split"
      (should= [[5 5] 1] (split [10 1])))

    (it "stops after first split"
      (should= [[5 5] 10] (split [10 10])))
    )

  (context "reduce"
    (it "reduces sample"
      (should= [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]]
               (reduce-sf [[[[[4, 3], 4], 4], [7, [[8, 4], 9]]], [1, 1]]))))

  (context "magnitude"
    (it "computes samples"
      (should= 29 (magnitude [9 1]))
      (should= 129 (magnitude [[9, 1], [1, 9]]))
      (should= 143 (magnitude [[1, 2], [[3, 4], 5]]))
      (should= 1384 (magnitude [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]]))

      )
    )
  )

(describe "solutions"
  (context "part 1"
    (it "solves samples"
      (should= [[[[1, 1], [2, 2]], [3, 3]], [4, 4]]
               (reduce add-sf [[1, 1]
                               [2, 2]
                               [3, 3]
                               [4, 4]]))
      (should= [[[[3, 0], [5, 3]], [4, 4]], [5, 5]]
               (reduce add-sf [[1, 1]
                               [2, 2]
                               [3, 3]
                               [4, 4]
                               [5, 5]]))

      (should= [[[[5, 0], [7, 4]], [5, 5]], [6, 6]]
               (reduce add-sf [[1, 1]
                               [2, 2]
                               [3, 3]
                               [4, 4]
                               [5, 5]
                               [6, 6]]))

      (should= [[[[4, 0], [5, 4]], [[7, 7], [6, 0]]], [[8, [7, 7]], [[7, 9], [5, 0]]]]
               (add-sf [[[0, [4, 5]], [0, 0]], [[[4, 5], [2, 6]], [9, 5]]]
                       [7, [[[3, 7], [4, 3]], [[6, 3], [8, 8]]]]))
      (should= [[[[8, 7], [7, 7]], [[8, 6], [7, 7]]], [[[0, 7], [6, 6]], [8, 7]]]
               (reduce add-sf [[[[0, [4, 5]], [0, 0]], [[[4, 5], [2, 6]], [9, 5]]]
                               [7, [[[3, 7], [4, 3]], [[6, 3], [8, 8]]]]
                               [[2, [[0, 8], [3, 4]]], [[[6, 7], 1], [7, [1, 6]]]]
                               [[[[2, 4], 7], [6, [0, 5]]], [[[6, 8], [2, 8]], [[2, 1], [4, 5]]]]
                               [7, [5, [[3, 8], [1, 4]]]]
                               [[2, [2, 2]], [8, [8, 1]]]
                               [2, 9]
                               [1, [[[9, 3], 9], [[9, 0], [0, 7]]]]
                               [[[5, [7, 4]], 7], 1]
                               [[[[4, 2], 2], 6], [8, 7]]]))

      (should= 4140
               (magnitude
                 (reduce add-sf [[[[0, [5, 8]], [[1, 7], [9, 6]]], [[4, [1, 2]], [[1, 4], 2]]]
                                 [[[5, [2, 8]], 4], [5, [[9, 9], 0]]]
                                 [6, [[[6, 2], [5, 6]], [[7, 6], [4, 7]]]]
                                 [[[6, [0, 7]], [0, 9]], [4, [9, [9, 0]]]]
                                 [[[7, [6, 4]], [3, [1, 3]]], [[[5, 5], 1], 9]]
                                 [[6, [[7, 3], [3, 2]]], [[[3, 8], [5, 7]], 4]]
                                 [[[[5, 4], [7, 7]], 8], [[8, 3], 8]]
                                 [[9, 3], [[9, 9], [6, [4, 9]]]]
                                 [[2, [[7, 7], 7]], [[5, 8], [[9, 3], [0, 2]]]]
                                 [[[[5, 2], 5], [8, [3, 7]]], [[5, [7, 5]], [4, 4]]]])))
      )

    (it "solves problem"
      (should= 4235 (solve-1 (slurp "input"))))
    )

  (context "part 2"
    (it "solves sample"
      (should= 3993 (solve-2 (slurp "sample"))))

    (it "solves problem"
      (should= 4659 (solve-2 (slurp "input"))))

    )
  )