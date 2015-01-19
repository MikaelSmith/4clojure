(defn xword [word b]
  (let [board (map (fn [x] (filter #(not= \space %) x)) b)
        match (fn [word [c & oll] i]
          (cond
           (and (>= i (count word)) (or (not c) (= c \#))) true
           (not c) false
           (or (= c (get word i)) (and (= c \_) (get word i))) (recur word oll (inc i))
           (= c \#) (recur word oll 0)
           :else (recur word (drop-while (partial not= \#) oll) 0)
           )
          )]
    (true? (some true?
                 (map #(match (vec word) % 0)
                      (concat board
                              ; Rotate the board so each entry is a column
                              (apply map vector board)
                              ))
    ))
   )
  )


(xword "the" ["_ # _ _ e"])
(xword "the" ["c _ _ _"
              "d _ # e"
              "r y _ _"])
(xword "joy" ["c _ _ _"
              "d _ # e"
              "r y _ _"])
(xword "joy" ["c o n j"
              "_ _ y _"
              "r _ _ #"])
