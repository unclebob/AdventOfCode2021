(ns day16.core)
(defn hexdigit->bits [digit]
  (condp = digit
    \0 "0000"
    \1 "0001"
    \2 "0010"
    \3 "0011"
    \4 "0100"
    \5 "0101"
    \6 "0110"
    \7 "0111"
    \8 "1000"
    \9 "1001"
    \A "1010"
    \B "1011"
    \C "1100"
    \D "1101"
    \E "1110"
    \F "1111"
    nil))

(defn hex->bits [hex]
  (apply str (mapcat hexdigit->bits hex)))

(defn- ->dec [bits]
  (loop [bits bits
         n 0]
    (if (empty? bits)
      n
      (recur (rest bits) (+ (* 2 n) (if (= \0 (first bits)) 0 1))))))

(defn- take-bits [n bits]
  (apply str (take n bits)))

(defn- drop-bits [n bits]
  (apply str (drop n bits)))

(defn get-literal [bits packet]
  (loop [bits bits
         literal ""
         length 0]
    (let [end? (= \0 (first bits))
          value (take-bits 4 (rest bits))
          literal (str literal value)
          length (+ length 5)]
      (if end?
        (assoc packet :value (->dec literal)
                      :length (+ (:length packet) length))
        (recur (drop 5 bits) literal length)))))

(declare get-packet)

(defn get-sub-packets-by-length [bits packet]
  (let [packet-length (:length packet)
        sub-packet-length (take-bits 15 bits)
        sub-packet-length (->dec sub-packet-length)
        packet (assoc packet :sub-length sub-packet-length
                             :length (+ packet-length
                                        sub-packet-length
                                        15))]
    (loop [bits (drop-bits 15 bits)
           sub-packets []
           n-bits-remaining sub-packet-length]
      (if (zero? n-bits-remaining)
        (assoc packet :sub-packets sub-packets)
        (let [sub-packet (get-packet bits)
              sub-packet-length (:length sub-packet)
              bits (drop-bits sub-packet-length bits)
              n-bits-remaining (- n-bits-remaining
                                  sub-packet-length)
              sub-packets (conj sub-packets sub-packet)]
          (recur bits sub-packets n-bits-remaining))))))

(defn- length-of [sub-packets]
  (reduce + (map :length sub-packets)))

(defn get-n-sub-packets [bits packet]
  (let [packet-length (:length packet)
        n-packets (take-bits 11 bits)
        n-packets (->dec n-packets)
        packet (assoc packet :n-packets n-packets)]
    (loop [bits (drop-bits 11 bits)
           sub-packets []
           packets-remaining n-packets]
      (if (zero? packets-remaining)
        (assoc packet :sub-packets sub-packets
                      :length (+ packet-length
                                 (length-of sub-packets)
                                 11))
        (let [sub-packet (get-packet bits)
              sub-packet-length (:length sub-packet)
              bits (drop-bits sub-packet-length bits)
              packets-remaining (dec packets-remaining)
              sub-packets (conj sub-packets sub-packet)]
          (recur bits sub-packets packets-remaining))))))

(defn get-operator [bits packet]
  (let [length-in-bits? (= \0 (first bits))
        packet-length (:length packet)
        remaining-bits (rest bits)
        packet (assoc packet :length (inc packet-length))]
    (if length-in-bits?
      (get-sub-packets-by-length remaining-bits packet)
      (get-n-sub-packets remaining-bits packet))))

(defn get-packet [bits]
  (let [version (->dec (.substring bits 0 3))
        type (->dec (.substring bits 3 6))
        packet {:version version :type type :length 6}
        remaining-bits (drop-bits 6 bits)]
    (if (= type 4)
      (get-literal remaining-bits packet)
      (get-operator remaining-bits packet))))

(defn parse-packet [hex]
  (get-packet (hex->bits hex)))

(defn- add-versions [packet]
  (let [version (:version packet)
        type (:type packet)]
    (if (= 4 type)
      version
      (loop [sub-packets (:sub-packets packet)
             sum version]
        (if (empty? sub-packets)
          sum
          (recur (rest sub-packets)
                 (+ sum (add-versions (first sub-packets)))))))))

(defn solve-1 [input]
  (let [packet (parse-packet input)]
    (add-versions packet)))

(declare evaluate)

(defn- gt [a b] (if (> a b) 1 0))
(defn- lt [a b] (if (< a b) 1 0))
(defn- eq [a b] (if (= a b) 1 0))


(defn evaluate [packet]
  (let [type (:type packet)
        args (map evaluate (:sub-packets packet))]
    (condp = type
      4 (:value packet)
      0 (reduce + args)
      1 (reduce * args)
      2 (apply min args)
      3 (apply max args)
      5 (apply gt args)
      6 (apply lt args)
      7 (apply eq args)
      nil
      )
    )
  )

(defn solve-2 [input]
  (let [packet (parse-packet input)]
    (evaluate packet)))

