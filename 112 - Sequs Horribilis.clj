(defn rsequs [n [x & coll]]
  (if (nil? x)
    [n []]
    (if (coll? x)
      (let [[n1 sub1] (rsequs n x)]
        (if (>= n1 0)
          (let [[n2 sub2] (rsequs n1 (rest coll))]
            [n2 (cons sub1 sub2)]
            )
          [n1 [sub1]]
          )
        )
      (if (>= n x)
        (let [[n1 sub1] (rsequs (- n x) coll)]
          [n1 (cons x sub1)]
          )
        [(- n x) []]
        )
      )
    )
  )

(defn sequs [n coll]
  (last (rsequs n coll))
  )

(sequs 10 [1 2 [3 [4 5] 6] 7])
(sequs 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
(sequs 9 (range))
(sequs 1 [[[[[1]]]]])
(sequs 0 [1 2 [3 [4 5] 6] 7])
(sequs 0 [0 0 [0 [0]]])
(sequs 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])

(#(letfn [(sequs [n [x & coll]]
  (if (nil? x)
    [n []]
    (if (coll? x)
      (let [[n1 sub1] (sequs n x)]
        (if (>= n1 0)
          (let [[n2 sub2] (sequs n1 (rest coll))]
            [n2 (cons sub1 sub2)]
            )
          [n1 [sub1]]
          )
        )
      (if (>= n x)
        (let [[n1 sub1] (rsequs (- n x) coll)]
          [n1 (cons x sub1)]
          )
        [(- n x) []]
        )
      )
    )
                )]
   (last (sequs %1 %2))
   ) 10 [1 2 [3 [4 5] 6] 7])
