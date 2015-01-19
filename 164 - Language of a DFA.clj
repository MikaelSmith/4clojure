; Need to define a generator that doesn't get stuck in an infinite loop when
; it encounters a loop. It needs to share generation with other paths.
; So process each step on all states in parallel.
(defn dfa [dfn]
  (letfn [(gen-step [dfn states]
                    (mapcat (fn [[s p]]
                              (filter (complement nil?) (map #(if-let [t ((dfn :transitions) s)]
                                                                (if-let [s* (t %)]
                                                                  (vector s* (str p %))))
                                                             (dfn :alphabet)
                                                             ))
                              )
                            states)
                    )
          (step [states]
                ; Return the accepted states, joined with
                ; a lazy-seq of calling step with each state processed
                ; for all next states.
                (if (empty? states)
                  []
                  (concat
                   (for [[s p] states
                         :when ((dfn :accepts) s)]
                     p)
                   (lazy-seq (step (gen-step dfn states)))
                   )
                  )
                )]
    (step [[(dfn :start) ""]])
    )
  )

(dfa '{:states #{q0 q1 q2 q3}
       :alphabet #{a b c}
       :start q0
       :accepts #{q1 q2 q3}
       :transitions {q0 {a q1}
                     q1 {b q2}
                     q2 {c q3}}})

(dfa '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
       :alphabet #{e h i l o y}
       :start q0
       :accepts #{q2 q4 q7}
       :transitions {q0 {h q1}
                     q1 {i q2, e q3}
                     q3 {l q5, y q4}
                     q5 {l q6}
                     q6 {o q7}}})

(dfa '{:states #{q0 q1 q2 q3 q4}
       :alphabet #{v w x y z}
       :start q0
       :accepts #{q4}
       :transitions {q0 {v q1, w q1, x q1, y q1, z q1}
                     q1 {v q2, w q2, x q2, y q2, z q2}
                     q2 {v q3, w q3, x q3, y q3, z q3}
                     q3 {v q4, w q4, x q4, y q4, z q4}}})

(take 20 (dfa '{:states #{q0 q1}
                :alphabet #{n m}
                :start q0
                :accepts #{q1}
                :transitions {q0 {n q0, m q1}}}))

