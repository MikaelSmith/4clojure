(defn __
  [collection]
  (letfn
    [(latin-sq?
      [coll]
      (let [v (sort (set (first coll)))]
        (and (not (empty? coll))
             (every? #(= v (sort %)) (rest coll))
             (every? #(= v (sort %)) (apply map vector coll)))
        )
      )
     (sub-sq
      [coll [x1 x2] [y1 y2]]
      (let [dx (- x2 x1)
            dy (- y2 y1)]
        (if (= dx dy)
          (let [rows (take dy (drop y1 coll))]
            (if (every? #(<= x2 (count %)) rows)
              (map #(take dx (drop x1 %)) rows)
              )
            )
          )
        )
      )
     (latin-sqs
      [coll maxlen]
      (let [width maxlen
            height (count coll)]
        (apply concat
         (for [i (range 2 (inc (min width height)))]
           (for [x (range (inc (- width i)))
                 y (range (inc (- height i)))
                 :let [subv (sub-sq coll [x (+ x i)] [y (+ y i)])]
                 :when (latin-sq? subv)]
             subv
             )
           )
               )
        )
      )
     (enum-aligns
      [coll xlen]
      (if (empty? coll)
        []
        (let [row (for [x (range (inc (- xlen (count (first coll)))))]
                    [(concat (repeat x nil) (first coll))])
              others (enum-aligns (rest coll) xlen)]
          (if (empty? others)
            row
            (for [a row
                  b others]
              (concat a b)
              )
            )
          )
        )
      )]
    (let [maxlen (apply max (map count collection))
          aligns (enum-aligns collection maxlen)
          sqs (set (filter (complement empty?) (mapcat #(latin-sqs % maxlen) aligns)))]
      (reduce (fn [m [k v]] (assoc m k v)) {}
              (map (fn [[k v]] [k (count v)]) (group-by count sqs)))
      )
    )
  )

(= (__ '[[A B C D]
         [A C D B]
         [B A D C]
         [D C A B]])
   {})

(= (__ '[[A B C D E F]
         [B C D E F A]
         [C D E F A B]
         [D E F A B C]
         [E F A B C D]
         [F A B C D E]])
   {6 1})

(= (__ '[[A B C D]
         [B A D C]
         [D C B A]
         [C D A B]])
   {4 1, 2 4})

(= (__ '[[B D A C B]
         [D A B C A]
         [A B C A B]
         [B C A B C]
         [A D B C A]])
   {3 3})

(= (__ '[[B D A C B]
         [D A B C A]
         [A B C A B]
         [B C A B C]
         [A D B C A]])
   {3 3})

(= (__ [  [2 4 6 3]
          [3 4 6 2]
          [6 2 4]  ])
   {})

(= (__ [[1]
        [1 2 1 2]
        [2 1 2 1]
        [1 2 1 2]
        []       ])
   {2 2})

(= (__ [[3 1 2]
        [1 2 3 1 3 4]
        [2 3 1 3]    ])
   {3 1, 2 2})

(time
 (= (__ [[8 6 7 3 2 5 1 4]
         [6 8 3 7]
         [7 3 8 6]
         [3 7 6 8 1 4 5 2]
         [1 8 5 2 4]
         [8 1 2 4 5]])
    {4 1, 3 1, 2 7})
 )
