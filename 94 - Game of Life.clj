(defn life [board]
  (let [rows (count board)
        cols (count (first board))
        cell (fn [b i j] (get (get b i []) j " "))]
    (for [i (range rows)]
      (apply str (for [j (range cols)]
        (let [neighbors (reduce +
                          (for [x [-1 0 1]
                                y [-1 0 1]
                                :when (not= x y 0)]
                            (if (= (cell board (+ i x) (+ j y)) \#) 1 0)))
              c (cell board i j)]
          (cond
           (and (= c \#) (or (< neighbors 2) (> neighbors 3))) \
           (and (= c \ ) (= neighbors 3)) \#
           :else c
           )
          )
        ))
      )
    )
  )

(def board
      ["      "
       " ##   "
       " ##   "
       "   ## "
       "   ## "
       "      "]
  )

(life board)
