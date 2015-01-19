(defn karn [sets]
  (let [; If s1 and s2 are boolean complements (they differ by only 1 flipped var)
        ; return the common values and the original sets.
        bool-complement (fn [[s1 s2]]
                          (let [x12 (clojure.set/difference s1 s2)
                                x21 (clojure.set/difference s2 s1)]
                            (if (and (= (count (clojure.set/difference s1 s2)) 1)
                                     (= (clojure.string/upper-case x12)
                                        (clojure.string/upper-case x21)))
                              [(clojure.set/intersection s1 s2) s1 s2]
                              )
                            )
                          )
        ; Return the common values of every boolean complement of pairing in sets,
        ; and sets with those removed.
        bool-reduce (fn [[sets acc]]
                      (let [; Filter the boolean complements for every pairing in sets.
                            comps (keep bool-complement (for [a sets, b (disj sets a)] [a b]))
                            ; Get the reduced boolean algebra.
                            x (set (map first comps))
                            ; Remove the complements from sets.
                            y (reduce #(disj % (second %2) (last %2)) sets comps)]
                        [x (clojure.set/union acc y)]
                        )
                      )
        ; Iteratively apply bool-reduce until no more reductions can be made.
        ; Return the last set generated.
        reduced-set (apply clojure.set/union
                           (last (take-while #(not (empty? (first %)))
                                             (iterate bool-reduce [sets #{}]))))
        ; Apply the reduced-set to the original sets.
        sets* (map (fn [x] (filter #(clojure.set/subset? % x) reduced-set)) sets)
        ]
    ; Return the set of boolean algebra that are uniquely required to satisfy sets.
    (set (map first (filter #(= (count %) 1) sets*)))
    )
  )

(karn #{#{'a 'B 'C 'd}
        #{'A 'b 'c 'd}
        #{'A 'b 'c 'D}
        #{'A 'b 'C 'd}
        #{'A 'b 'C 'D}
        #{'A 'B 'c 'd}
        #{'A 'B 'c 'D}
        #{'A 'B 'C 'd}})
