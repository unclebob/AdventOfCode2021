(ns day19.core-spec
  (:require [speclj.core :refer :all]
            [day19.core :refer :all]))

(describe "parsing"
  (context "lines"
    (it "parses coordinates"
      (should= [7 -55 2] (parse-line "7,-55,2")))

    (it "parses scanner"
      (should= 99 (parse-line "--- scanner 99 ---")))

    (it "fails gracefully"
      (should= nil (parse-line "-- 22 -- 31 --"))))

  (context "scanner reports"
    (it "gathers two reports"
      (should= {0 [[1 2 3] [4 5 6]]
                1 [[7 8 9] [10 11 12]]}
               (parse-input (str
                              "-- 0 --\n"
                              "1,2,3\n"
                              "4,5,6\n"
                              "\n"
                              "-- 1 --\n"
                              "7,8,9\n"
                              "10,11,12\n"))))))
(def beacons [[5 5 1] [8 5 1] [4 7 1] [7 8 1]])
(def scanner [[2 1 1] [5 1 1] [1 3 1] [4 4 1]])
(describe "utilities"
  (context "rotations"
    (it "rotates around axis"
      (should= [[3 2] [2 -3] [-3 -2] [-2 3]]
               (rotate-axis [3 2])))

    (it "rotates around x axis"
      (should= [[1 2 3] [1 3 -2] [1 -2 -3] [1 -3 2]]
               (rotations-x [1 2 3])))

    (it "rotates around y axis"
      (should= [[1 2 3] [3 2 -1] [-1 2 -3] [-3 2 1]]
               (rotations-y [1 2 3])))

    (it "rotates around z axis"
      (should= [[1 2 3] [2 -1 3] [-1 -2 3] [-2 1 3]]
               (rotations-z [1 2 3])))

    (it "finds all orientations of a point"
      (should= 24 (count (orientations [1 2 3]))))

    (it "finds all orientations of a scanner"
      (should= 24
               (count (scanner-orientations [[1 2 3] [4 5 6]])))

      (should= [[1 2 3] [4 5 6]]
               (first (scanner-orientations [[1 2 3] [4 5 6]]))))
    )

  (context "translations"
    (it "translates a scanner"
      (should= [[2 5 8] [5 7 9]] (translate-scanner [1 2 3] [[1 3 5] [4 5 6]]))))

  (context "coincidence detection"
    (it "finds coincident points"
      (should= #{[4 5 6]}
               (coincidences [[1 2 3] [4 5 6] [7 8 9]]
                             [[1 3 5] [4 5 6] [9 8 7]]))
      (should= #{[1 2 3] [4 5 6]}
               (coincidences [[1 2 3] [4 5 6] [7 8 9]]
                             [[1 2 3] [4 5 6] [9 8 7]])))

    (it "finds all offsets"
      (should= #{[-1 6 0] [7 2 0] [6 5 0] [2 7 0]
                 [4 2 0] [1 1 0] [0 3 0] [3 4 0]
                 [2 6 0] [0 4 0] [5 7 0] [6 4 0] [4 1 0]}
               (all-offsets beacons scanner)))

    (it "finds best translation"
      (let [[translation offset] (find-best-translation beacons scanner 4)]
        (should= #{[8 5 1] [7 8 1] [5 5 1] [4 7 1]} (set translation))
        (should= [3 4 0] offset)))

    (it "finds best orientation"
      (let [[points offset] (find-best-orientation
                              beacons
                              (last (scanner-orientations scanner))
                              4)]
        (should= [3 4 0] offset))
      )
    )
  )

(def sample-offsets
  [[68 -1246 -43]
   [-92 -2380 -20]
   [-20 -1133 1061]
   [1105 -1205 1229]])

(def solution-offsets
  [[107 -1107 -103]
   [-12 -1098 1067]
   [38 1213 -102]
   [-1199 -1205 1054]
   [-59 -2331 -114]
   [-39 1336 -1209]
   [-1221 1246 -1298]
   [-1203 -2354 1028]
   [-24 -2345 -1388]
   [-11 -2328 -2513]
   [-1155 -2289 -1267]
   [1137 -2354 -185]
   [99 -1188 -2466]
   [-1112 -1126 -2573]
   [-1212 -1125 -1217]
   [-2422 -1229 -1332]
   [-2342 -2258 -1213]
   [-1242 -2316 -20]
   [-1087 -2382 -2510]
   [-2397 1350 -1273]
   [-1147 2470 -1367]
   [-2386 -1133 -2447]
   [-1234 -19 -1347]
   [-1247 2471 -2553]
   [-1137 -2335 -3610]
   [-58 2432 -2497]
   [-1077 -1232 -3760]
   [-2299 -3615 -1274]
   [-1226 33 -3681]
   [-1086 3712 -1326]
   [-2301 3723 -1377]])

(describe "solutions"
  (context "check against sample data"
    (it "should find common points between scanners 0 and 1"
      (let [scanners (parse-input (slurp "sample"))
            s0 (scanners 0)
            s1 (scanners 1)
            s4 (scanners 4)
            [_ s1-0-offset] (find-best-orientation s0 s1 12)]
        (should= [68 -1246 -43] s1-0-offset)))
    )

  ;(context "part 1"
  ;  (it "solves sample"
  ;    (should= 79 (count (solve-1 (parse-input (slurp "sample"))))))
  ;
  ;  (it "solves problem"
  ;    (should= 0 (count (solve-1 (parse-input (slurp "input"))))))
  ;  )

  (context "part 2"
    (it "solves sample"
      (should= 3621 (solve-2 sample-offsets)))

    (it "solves problem"
      (should= 10707 (solve-2 solution-offsets)))))
