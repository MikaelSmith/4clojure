(defn travel
  "
  Find the location of M.
  Then, build a set of all nodes connected to M, working out from
  recently added nodes.
  If C is reached, return true. Once no more nodes can be added, return false.
  "
  [maze]
  (let [; Find the starting mouse (M)
        start (loop [i 0]
                (let [j (.indexOf (nth maze i) "M")]
                  (if (not= j -1) [i j] (recur (inc i)))))
        ; Find adjacent open squares or the cheese
        adjacent (fn [[x y]]
                   (filter
                    #(let [it (get-in maze %)] (or (= it \ ) (= it \C)))
                    [[x (inc y)] [(inc x) y] [x (dec y)] [(dec x) y]]))
        ]
    (loop [; Nodes already visited
           visited #{}
           ; Next nodes to visit
           candidates #{start}
           ]
      (if (empty? candidates)
        false
        (let [; Current set of visited nodes
              newvisited (into visited candidates)
              ; Find new candidates from current candidates
              newcand
              (clojure.set/difference
               (set (mapcat adjacent candidates)) newvisited)
              ]
          (if (empty? (filter #(= (get-in maze %) \C) newcand))
            (recur newvisited newcand)
            true
            )
          )
        )
      )
    )
  )

(def M ["#######"
        "#     #"
        "#  #  #"
        "#M # C#"
        "#######"])

(travel ["M   C"])
(travel ["M # C"])
(travel M)
(travel ["########"
         "#M  #  #"
         "#   #  #"
         "# # #  #"
         "#   #  #"
         "#  #   #"
         "#  # # #"
         "#  #   #"
         "#  #  C#"
         "########"])

(defn adjacent [maze [x y]]
  (filter
   #(let [item (get-in maze %)] (or (= item \ ) (= item \C)))
   [[x (inc y)] [(inc x) y] [x (dec y)] [(dec x) y]])
  )

(adjacent M [3 1])
