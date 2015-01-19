(defn __ [h]
  (let [card (fn [[s r]]
               {:suit ({\D :diamond \H :heart \C :club \S :spade} s)
                :rank (.indexOf (seq "23456789TJQKA") r)})
        straight? (fn [hand]
                    ; Try with A as both -1 and 12
                    (let [parts (map #(list (%1 :rank) %2)
                                     (sort-by #(% :rank) hand) (range 5))]
                      (or (= (count (partition-by #(apply - %) parts)) 1)
                          (= (count (partition-by #(apply - %)
                                                  (cons (list (- (first (last parts)) 12) 0)
                                                        (butlast parts)))) 1))
                      ))
        flush? (fn [hand] (apply = (map #(% :suit) hand)))
        n-kind? (fn [n hand] (some #(= n (count %)) (vals (group-by #(% :rank) hand))))
        two-pair? (fn [hand] (let [groups (group-by #(% :rank) hand)]
                               (= 2 (count (filter #(= 2 (count %)) (vals groups))))))
        hand (map card h)]
    (cond
     (and (straight? hand) (flush? hand)) :straight-flush
     (n-kind? 4 hand) :four-of-a-kind
     (and (n-kind? 3 hand) (n-kind? 2 hand)) :full-house
     (flush? hand) :flush
     (straight? hand) :straight
     (n-kind? 3 hand) :three-of-a-kind
     (two-pair? hand) :two-pair
     (n-kind? 2 hand) :pair
     :else :high-card
     )
    )
  )

(= :high-card (__ ["HA" "D2" "H3" "C9" "DJ"]))
(= :pair (__ ["HA" "HQ" "SJ" "DA" "HT"]))
(= :two-pair (__ ["HA" "DA" "HQ" "SQ" "HT"]))
(= :three-of-a-kind (__ ["HA" "DA" "CA" "HJ" "HT"]))
(= :straight (__ ["HA" "DK" "HQ" "HJ" "HT"]))
(= :straight (__ ["HA" "H2" "S3" "D4" "C5"]))
(= :flush (__ ["HA" "HK" "H2" "H4" "HT"]))
(= :full-house (__ ["HA" "DA" "CA" "HJ" "DJ"]))
(= :four-of-a-kind (__ ["HA" "DA" "CA" "SA" "DJ"]))
(= :straight-flush (__ ["HA" "HK" "HQ" "HJ" "HT"]))

