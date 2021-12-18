(ns day16.core-spec
  (:require [speclj.core :refer :all]
            [day16.core :refer :all]))

(describe "hex to bits"
  (it "translates single digits"
    (should= "1010" (hex->bits "A"))
    (should= "0000" (hex->bits "0"))
    (should= "1111" (hex->bits "F"))
    (should= "" (hex->bits "G")))

  (it "translates strings"
    (should= "10101111" (hex->bits "AF"))
    (should= "11011110101011011011111011101111"
             (hex->bits "DEADBEEF")))
  )

(describe "packet parsing"
  (it "decodes literals"
    (should= {:version 6 :type 4 :value 2021 :length 21}
             (parse-packet "D2FE28")))

  (context "operators"
    (it "decodes bit-length operator"
      (should=
        {:version 1
         :type 6
         :length 49
         :sub-length 27
         :sub-packets [{:version 6 :type 4 :value 10 :length 11}
                       {:version 2 :type 4 :value 20 :length 16}]}
        (parse-packet "38006F45291200")))

    (it "decodes n-packet operator"
      (should=
        {:version 7
         :type 3
         :length 51
         :n-packets 3
         :sub-packets [{:version 2 :type 4 :value 1 :length 11}
                       {:version 4 :type 4 :value 2 :length 11}
                       {:version 1 :type 4 :value 3 :length 11}]}
        (parse-packet "EE00D40C823060")))

    (it "does sums"
      (should= 3 (evaluate (parse-packet "C200B40A82"))))

    (it "does products"
      (should= 54 (evaluate (parse-packet "04005AC33890"))))

    (it "does mins"
      (should= 7 (evaluate (parse-packet "880086C3E88112"))))

    (it "does maxes"
      (should= 9 (evaluate (parse-packet "CE00C43D881120"))))

    (it "does < and >"
      (should= 1 (evaluate (parse-packet "D8005AC2A8F0")))
      (should= 0 (evaluate (parse-packet "F600BC2D8F"))))

    (it "does ="
      (should= 0 (evaluate (parse-packet "9C005AC2F8F0"))))

    (it "does combos"
      (should= 1 (evaluate (parse-packet "9C0141080250320F1802104A08"))))
    )
  )

(describe "solution"
  (context "part 1"
    (it "solves problem"
      (should= 1007 (solve-1 (slurp "input")))))

  (context "part 2"
    (it "solves problem"
      (should= 834151779165 (solve-2 (slurp "input")))))
  )
