(def ith
    (fun [ix xs]
        (if [atom? xs]
            xs
            (if ix
                (recur (- ix 1) (cdr xs))
                (car xs)))))
(ith 5 (cons 3 (cons 2 (cons 4 (cons 5 (cons 1 0))))))
