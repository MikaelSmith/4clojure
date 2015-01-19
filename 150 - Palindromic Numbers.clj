(defn pal [start]
  (let [pal? (fn [n]
               (let [dig (if (coll? n) n (str n))
                     mid (Math/ceil (/ (count dig) 2))]
                 (= (take mid dig) (take mid (reverse dig)))))
        ; Convert a collection of numbers to a single integer.
        toInt (fn [coll]
                (loop [acc 0
                       scale 1
                       [n & ums :as nums] coll]
                  (if (empty? nums)
                    acc
                    (recur (+ acc (* scale n)) (* scale 10) ums)
                    )))
        ; Construct a palindrome from the unique half, with final length n.
        make-pal (fn [part n]
                   (if (even? n)
                     (toInt (concat (reverse part) part))
                     (toInt (concat (reverse part) (rest part)))))
        ; Find the next number that's a palindrome.
        next-pal (fn [p]
                   (let [dig (map #(- (int %) (int \0)) (str p))
                         n (count dig)]
                     (loop [acc []
                            [s & e :as seed] (drop (int (/ n 2))
                                                   (reverse dig))]
                       (if (empty? seed)
                         (make-pal acc n)
                         (if (= s 9)
                           (if (empty? e)
                             ; At the end, use 1 instead of 0 and
                             ; prefix with 0 if going from even to odd.
                             (if (even? n)
                               (make-pal (concat [0] acc [1]) (inc n))
                               (make-pal (conj acc 1) (inc n))
                               )
                             (recur (conj acc 0) e)
                             )
                           (make-pal (concat acc (cons (inc s) e)) n)
                           )
                         )
                       )
                     )
                   )
        ; The first palindrome.
        s (if (pal? start)
            start
            (let [dig (map #(- (int %) (int \0)) (str start))
                  part (take (int (/ (count dig) 2)) dig)
                  x (make-pal (reverse part) (count dig))]
              (if (>= x start)
                x
                (next-pal start)
                )
              ))]
    (cons s (lazy-seq (pal (next-pal s))))
    )
  )

(take 16 (pal 162))
(nth (pal 0) 10101)
(take 6 (pal 1234550000))
