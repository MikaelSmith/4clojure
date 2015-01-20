(defn parens
  "Adds a parenthesis pair to each string in the set found,
  until n parenthesis pairs exist in each string"
  ([n] (parens "" n 0 0))
  ([s n open close]
   (if (= n close)
     #{s}
     (clojure.set/union
      (if (< open n)
        (parens (str s "(") n (inc open) close)
        #{})
      (if (< close open)
        (parens (str s ")") n open (inc close))
        #{})
      ))
   )
  )

(= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (parens n)) [0 1 2]))
(= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (parens 3))
(= 16796 (count (parens 10)))
(= (nth (sort (filter #(.contains ^String % "(()()()())") (parens 9))) 6) "(((()()()())(())))")
(= (nth (sort (parens 12)) 5000) "(((((()()()()()))))(()))")
