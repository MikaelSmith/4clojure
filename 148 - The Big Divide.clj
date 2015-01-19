; (sum 1 to a*) * a, where a* is (n-1)/a
; + (sum 1 to b*) * b, where b* is (n-1)/b
; - (sum 1 to ab*) * a*b, where ab* is (n-1)/(a*b)
; (sum 1 to x) = x*(x+1)/2
(defn bigdiv [n a b]
  (letfn [(sum-x-by-y [x y]
                      (let [x* (bigint (/ (dec x) y))]
                        (* (/ (* x* (inc x*)) 2) y)))]
    (- (+ (sum-x-by-y n a) (sum-x-by-y n b)) (sum-x-by-y n (* a b)))
    )
  )

(bigdiv 3 17 11)
(bigdiv 1000 3 5)
(bigdiv 10 3 5)
(bigdiv 100000000 3 5)
