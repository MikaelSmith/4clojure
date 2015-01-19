(defn brackets
  ([s] (brackets s '()))
  ([s b]
   (let [not-braces (complement #{\{ \[ \( \} \] \)})
         open-brace #{\{ \[ \(}
         close-brace #{\} \] \)}
         brace-map {\{ \}, \[ \], \( \)}]
   (cond
    ; String is done, return true if no open pairs.
    (empty? s) (empty? b)
    ; No open pairs, look for a start.
    (empty? b) (let [nexts (drop-while not-braces s)]
              (if (close-brace s)
                false
                (recur (rest nexts) (take 1 nexts))
                )
              )
    ; Open pair, look for its counterpart, an error, or a new layer.
    :else (let [nexts (drop-while not-braces s)]
            (cond
             (empty? nexts) false
             (= (brace-map (first b)) (first nexts)) (recur (rest nexts) (rest b))
             (close-brace (first nexts)) false
             :else (recur (rest nexts) (conj b (first nexts)))
             )
            )
    )
     )
   )
  )

(defn brackets [s]
  (empty?
   (reduce
    (fn [[left & other :as stack] right]
      (cond
       ; New char closes the inner-most open bracket -> pop inner-most.
       (= right ({\[ \], \( \), \{ \}} left)) other
       ; New char opens a new bracket -> push new bracket.
       ((set "{}[]()") right) (cons right stack)
       ; Not a bracket.
       :else stack
       )
      )
    ()
    s
    )
   )
  )

(brackets "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }")
(brackets "This string has no brackets.")
(brackets "(start, end]")
(brackets "())")
(brackets "[ { ] } ")
(brackets "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
