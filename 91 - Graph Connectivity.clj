(defn connected [graph]
  (loop [[[a b :as e] & edges] (sort (concat graph (mapv (fn [[x y]] [y x]) graph)))
         unmatched #{}
         nodes #{a}]
    (cond
     (or (contains? nodes a) (contains? nodes b))
       (recur edges (disj unmatched e) (into nodes e))
     (empty? edges) (empty? unmatched)
     :else (recur edges (conj unmatched e) nodes)
     )
    )
  )

(connected #{[:a :a]})
(connected #{[:a :b]})
(connected #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4]})
(connected #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4] [3 4]})
(connected #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e]})
(connected #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e] [:x :a]})
