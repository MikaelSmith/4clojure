(defn ps [s]
  (reduce #(into %1 (map conj %1 (repeat %2))) #{#{}} s)
  )

(defn sum-subsets
           [coll]
  (let [subsets (disj (ps coll) #{})]
    (map #(apply + %) subsets)
    )
  )

(defn ssss [& sets]
  (letfn []
    (not
     (empty?
      (apply clojure.set/intersection
             (map #(into #{} (sum-subsets %)) sets)
             ))))
  )

(ssss #{-1 1 99}
      #{-2 2 888}
      #{-3 3 7777})
(ssss #{1} #{2} #{3} #{4})
(ssss #{1})
(ssss #{-10 9 -8 7 -6 5 -4 3 -2 1} #{10 -9 8 -7 6 -5 4 -3 2 -1})

(ps #{1 2 3 4})
