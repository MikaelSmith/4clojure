(defn length [line]
  (if (zero? line)
    0
    (int (Math/ceil (/ (Math/log line) (Math/log 2))))
    )
  )

; Return the length of the contiguous line starting at line.
(defn sub-len [line i]
  (loop [i* i
         l  0]
    (if (bit-test line i*)
      (recur (inc i*) (inc l))
      l
      )
    )
  )
(= (sub-len 15 0) 4)
(= (sub-len 15 3) 1)


; Return the start and length of the longest contiguous line.
(defn contig [line]
  (let [len (length line)]
    (loop [i 0
           longest [0 0]]
      (if (>= i len)
        longest
        (let [l (sub-len line i)]
          (recur (+ i (max 1 l))
                 (if (> l (last longest))
                   [i l]
                   longest
                   )
                 )
          )
        )
      )
    )
  )
(= (contig 15) [0 4])
(= (contig 7) [0 3])
(= (contig 22) [1 2])

; Rotate a grid by 90 degrees
(defn rot [coll]
  (let [len (reduce max (map length coll))]
    (loop [acc (repeat len 0)
           i 0]
      (if (= i (count coll))
        acc
        (recur
         (map
          #(if (bit-test (nth coll i) %2)
             (bit-set %1 i)
             %1
             )
          acc
          (range len)
          )
         (inc i)
         )
        )
      )
    )
  )
(= (rot [15 15 15 15 15]) [31 31 31 31])
(= (rot [1 3 7 15 31]) [31 30 28 24 16])

(defn triangle [coll]
  (let [area
        (reduce
         max
         (mapcat
          #(let [len (reduce max (map length coll))]
             (for [i (range (count %))
                   j (range len)]
               (let [tri
                     (for [x (range i (count %))
                           :let [x* (- (inc x) i)]
                           :while (<= x* (sub-len (nth % x) j))]
                       x*
                       )
                     iso
                     (for [x (range (count tri))
                           :let [x* (- (count tri) (inc x))
                                 off (+ i (count tri) x)]
                           :while (and (< off (count %))
                                       (<= x* (sub-len (nth % off) j)))]
                       x*
                       )]
                 (reduce +
                         (if (= (count iso) (dec (count tri)))
                           (concat tri iso)
                           tri
                           )
                         )
                 )
               )
             )
          [coll (reverse coll) (rot coll) (reverse (rot coll))]
          )
         )
        ]
    (if (< area 3)
      nil
      area)
    )
  )

(triangle [15 15 15 15 15])

(triangle [1 3 7 15 31])

(triangle [3 3])

(triangle [7 3])

(triangle [17 22 6 14 22])

(triangle [18 7 14 14 6 3])

(triangle [21 10 21 10])

(triangle [0 31 0 31 0])
