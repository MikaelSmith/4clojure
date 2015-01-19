(defn tricky [trump]
  (fn [coll]
    (reduce
     #(cond
       (= (%1 :suit) (%2 :suit)) (max-key (fn [card] (card :rank)) %1 %2)
       (= (%1 :suit) trump) %1
       (= (%2 :suit) trump) %2
       :else %1
       )
     coll)
    )
  )

((tricky :club) [{:suit :spade :rank 2} {:suit :club :rank 10}])
