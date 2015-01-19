(defn roman [n]
  (let [nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
        pairs {"IV" 4, "IX" 9, "XL" 40, "XC" 90, "CD" 400, "CM" 900}]
    (loop [s n
           v 0]
      (cond
       (empty? s) v
       (pairs (apply str (take 2 s))) (recur (drop 2 s) (+ v (pairs (apply str (take 2 s)))))
       :else (recur (rest s) (+ v (nums (first s))))
       )
      )
    )
  )

(roman "XIV")
(roman "XLVIII")
