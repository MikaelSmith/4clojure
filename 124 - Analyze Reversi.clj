(defn reversi [rows turn]
  (let [op '{b w, w b}
        moves {(list turn (op turn) 'e 'e) [2 [1]]
               (list turn (op turn) 'e turn) [2 [1]]
               (list 'e turn (op turn) 'e) [3 [2]]
               (list turn turn (op turn) 'e) [3 [2]]
               (list turn (op turn) (op turn) 'e) [3 [1 2]]
               (list 'e (op turn) turn 'e) [0 [1]]
               (list 'e (op turn) turn turn) [0 [1]]
               (list 'e 'e (op turn) turn) [1 [2]]
               (list turn 'e (op turn) turn) [1 [2]]
               (list 'e (op turn) (op turn) turn) [0 [1 2]]
               }
        cols (apply map vector rows)
        diag (map #(get-in rows [% %]) [0 1 2 3])
        rdiag (map #(get-in rows [% (- 3 %)]) [0 1 2 3])
        dp (map #(get-in rows [% (inc %)]) [0 1 2])
        dn (map #(get-in rows [(inc %) %]) [0 1 2])
        rp (map #(get-in rows [% (- 2 %)]) [0 1 2])
        rn (map #(get-in rows [(- 2 %) %]) [0 1 2])
        ]
    (merge-with
     #(clojure.set/join %1 %2)
     (into {}
           (for [i (range 4)
                 :let [x (moves (nth rows i))]
                 :when x]
             {[i (first x)] (set (map #(vector i %) (last x)))}
             ))
     (into {}
           (for [i (range 4)
                 :let [x (moves (nth cols i))]
                 :when x]
             {[(first x) i] (set (map #(vector % i) (last x)))}
             ))
     (if-let [x (moves diag)]
       {[(first x) (first x)] (set (map #(vector % %) (last x)))}
       {}
       )
     (if-let [x (moves rdiag)]
       {[(first x) (- 3 (first x))] (set (map #(vector % (- 3 %)) (last x)))}
       {}
       )
     (if-let [x (moves dp)]
       {[(first x) (inc (first x))] (set (map #(vector % (inc %)) (last x)))}
       {}
       )
     (if-let [x (moves dn)]
       {[(inc (first x)) (first x)] (set (map #(vector (inc %) %) (last x)))}
       {}
       )
     (if-let [x (moves rp)]
       {[(first x) (- 2 (first x))] (set (map #(vector % (- 2 %)) (last x)))}
       {}
       )
     (if-let [x (moves rn)]
       {[(- 2 (first x)) (first x)] (set (map #(vector (- 2 %) %) (last x)))}
       {}
       )
     )
    )
  )

(reversi '[[e e e e]
           [e w b e]
           [e b w e]
           [e e e e]] 'w)

(reversi '[[e e e e]
           [e w b e]
           [w w w e]
           [e e e e]] 'b)

(reversi '[[e e e e]
           [e w b e]
           [w w b e]
           [e e b e]] 'w)

(reversi '[[e e w e]
          [b b w e]
          [b w w e]
          [b w w w]] 'b)
