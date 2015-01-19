(#(letfn [(make-lev []
          (with-local-vars
            [lev (memoize
                  (fn [s t]
  (cond
   (empty? s) (count t)
   (empty? t) (count s)
   :else (let [cost (if (= (last s) (last t)) 0 1)]
           (min (inc (lev (drop-last s) t))
                (inc (lev s (drop-last t)))
                (+ (lev (drop-last s) (drop-last t)) cost))
           )
   )
                             ))]
            (.bindRoot lev @lev)
            @lev))]
    (let [lev (make-lev)]
      (lev %1 %2)
      )
  ) "ttttattttctg" "tcaaccctaccat")

(defn lev [s t]
  (cond
   (= s t) 0
   (empty? s) (count t)
   (empty? t) (count s)
   :else
   (loop [v0 (range (inc (count t)))
          i 0]
     (if (= i (count s))
       (last v0)
       (recur
        (loop [v1 [(inc i)]
               j 0]
          (if (= j (count t))
            v1
            (recur
             (conj v1
                   (min (inc (last v1))
                        (inc (nth v0 (inc j)))
                        (+ (nth v0 j) (if (= (nth s i) (nth t j)) 0 1))))
             (inc j)
             )
            )
          )
        (inc i)
        )
       )
     )
   )
  )

(lev "kitten" "sitting")
(lev "closure" "clojure")
(lev "xyx" "xyyyx")
(lev "" "123456")
(lev "Clojure" "Clojure")
(lev [1 2 3 4] [0 2 3 4 5])
(lev "ttttattttctg" "tcaaccctaccat")
