"expand-shape - expand a diamond-shape on one side, determined by rt"
(defn expand-shape [coll rt]
  (letfn [(spaces [n] (apply str (repeat n \ )))
          (expand-above [c]
                        (for [j (range 1 (* rt 2))]
                          (cond
                           (= j 1) (str (spaces (dec rt)) "*" (spaces (dec rt)))
                           (= j 2) (str (spaces (- rt 2)) "* *" (spaces (- rt 2)))
                           (<= j rt) (let [j* (- j 3)]
                                       (str (spaces (- rt j))
                                            "* "
                                            (clojure.string/trim (apply str (nth c j*)))
                                            " *"
                                            (spaces (- rt j))))
                           :else (str " " (apply str (nth c (- j 3))) " ")
                           )))]
    (if (odd? rt)
      ; On odd square root, add above, on even add below.
      (expand-above coll)
      (reverse (expand-above (reverse coll)))
     )
    )
  )

"Unit tests for expand-shape"
(= (expand-shape ["   0   "
                  "  1 0  "
                  " 0 1 0 "
                  "* 0 0 0"
                  " * 1 * "
                  "  * *  "
                  "   *   "] 5) ["    *    "
                                 "   * *   "
                                 "  * 0 *  "
                                 " * 1 0 * "
                                 "* 0 1 0 *"
                                 " * 0 0 0 "
                                 "  * 1 *  "
                                 "   * *   "
                                 "    *    "]   )

(= (expand-shape ["  6  "
                  " 5 * "
                  "2 2 *"
                  " 6 4 "
                  "  1  "] 4) ["   6   "
                               "  5 *  "
                               " 2 2 * "
                               "* 6 4 *"
                               " * 1 * "
                               "  * *  "
                               "   *   "])
(= (expand-shape [[\ \2\ ]
                  [\*\ \4]
                  [\ \*\ ]] 3) ["  *  "
                                " * * "
                                "* 2 *"
                                " * 4 "
                                "  *  "])

"add-digit - replace a * with a character in clock-wise order"
(defn add-digit [coll d]
  (let [len (count coll)
        mid (int (/ len 2))
        stars? (fn [s] (re-find (re-matcher #"\*" (str s))))
        fill-star (fn fs [[s & e]]
                    (if (stars? s)
                      (cons (clojure.string/replace (apply str s) \* d) e)
                      (cons s (fs e))
                      ))]
    (cond
     ; If the top-half contains a *, then fill in from the left-most *.
     (some stars? (take mid coll))
      (apply map str (fill-star (apply map str coll)))
     ; If the bottom-half contains a *, then fill in from the right-most *.
     (some stars? (drop (inc mid) coll))
      (apply map str (reverse (fill-star (reverse (apply map str coll)))))
     ; Else only one * exists in the middle row, replace it.
     :else (concat (take mid coll)
                   (fill-star (drop mid coll)))
     )
    )
  )

"Unit tests for add-digit"
(= (add-digit ["  6  "
               " 5 * "
               "2 2 *"
               " 6 4 "
               "  1  "] \6) ["  6  "
                             " 5 6 "
                             "2 2 *"
                             " 6 4 "
                             "  1  "])
(= (add-digit ["   0   "
               "  1 0  "
               " 0 1 0 "
               "* 0 0 0"
               " * 1 * "
               "  * *  "
               "   *   "] \1) ["   0   "
                               "  1 0  "
                               " 0 1 0 "
                               "* 0 0 0"
                               " * 1 1 "
                               "  * *  "
                               "   *   "])
(= (add-digit ["  6  "
               " 5 6 "
               "2 2 *"
               " 6 4 "
               "  1  "] \5) ["  6  "
                             " 5 6 "
                             "2 2 5"
                             " 6 4 "
                             "  1  "])
(= (add-digit [[\ \2\ ]
               [\*\ \4]
               [\ \*\ ]] \3) [" 2 "
                              "* 4"
                              " 3 "])

(defn square? [n]
  (= n (let [r (int (Math/sqrt n))] (* r r))))

(defn shape [coll]
  (loop [[d & etc] coll
         acc ["*"]
         i 1]
    (if (nil? d)
      acc
      (recur etc
             (if (and (square? (dec i)) (> i 1))
               ; If the last index was a perfect square, then start a new section.
               (add-digit (expand-shape acc (inc (int (Math/sqrt (dec i))))) d)
               (add-digit acc d)
              )
             (inc i))
      )
    )
  )

(= (shape "24") [" 2 " "* 4" " * "])

(defn sq-sq [start end]
  (let [squares (take-while (partial >= end) (iterate #(* % %) start))
        digits (apply str squares)]
    (shape digits)
    )
  )

(sq-sq 2 2)
(sq-sq 2 4)
(sq-sq 3 81)
(sq-sq 4 20)
(sq-sq 2 256)
(sq-sq 10 10000)
