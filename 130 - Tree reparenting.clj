(defn reparent
  ([root tree]
   (if (= (first tree) root) tree (reparent root tree '())))
  ([root tree past]
   (if (= (first tree) root)
     (concat tree [past])
     (some identity
           (for [i (range 1 (count tree))]
             (reparent root
                       (nth tree i)
                       (if (empty? past)
                         (concat (take i tree) (drop (inc i) tree))
                         (concat (take i tree) (drop (inc i) tree) [past])
                         )
                       )
             )
           )
     )
   )
  )

(reparent 'n '(n))
(reparent 'a '(t (e) (a)))
(reparent 'e '(a (t (e))))
(reparent 'a '(c (b (a))))
(reparent 'd '(a (b (c) (d) (e)) (f (g) (h))))
(reparent 'c '(a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o)))))
