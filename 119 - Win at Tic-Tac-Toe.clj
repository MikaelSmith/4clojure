(defn ttt [p board]
  (letfn [(M [b i j] (get-in b [i j]))
          (win [b]
               (let [triples (concat
                              b
                              (apply map vector b)
                              [[(M b 0 0) (M b 1 1) (M b 2 2)]
                               [(M b 0 2) (M b 1 1) (M b 2 0)]])]
                 (some (partial every? (partial = p)) triples)
                 ))]
    (set (for [i (range 3)
          j (range 3)
          :when (and (= (M board i j) :e) (win (assoc-in board [i j] p)))]
      [i j]
      ))
    )
  )

(def b1 [[:o :e :e]
         [:o :x :o]
         [:x :x :e]])
(ttt :x b1)
