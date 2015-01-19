(defn row [f i j]
  (lazy-seq (cons (f i j) (row f i (inc j))))
  )
(defn col [f i j]
  (lazy-seq (cons (row f i j) (col f (inc i) j)))
  )

(defn infm
  ([f] (infm f 0 0))
  ([f m n] (col f m n))
  ([f m n s t] (take s (map #(take t %) (infm f m n))))
  )

(take 5 (map #(take 6 %) (infm str)))
(take 6 (map #(take 5 %) (infm str 3 2)))
(infm * 3 5 5 7)
(infm #(/ % (inc %2)) 1 0 6 4)
(class (infm (juxt bit-or bit-xor)))
(class (nth (infm (constantly 10946)) 34))



(fn infm
  ([f] (infm f 0 0))
  ([f m n]
   (letfn [(row [f i j] (lazy-seq (cons (f i j) (row f i (inc j)))))
           (col [f i j] (lazy-seq (cons (row f i j) (col f (inc i) j))))]
     (col f m n)))
  ([f m n s t] (take s (map #(take t %) (infm f m n))))
  )
