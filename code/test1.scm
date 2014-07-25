(def fact
    (fun [n]
        (let (
            [aux
                (fun [x acc]
                    (if (= x n)
                        acc
                        (recur (+ 1 x) (* x acc))))]
            )
            (aux 1 INIT-FACT))))
(def INIT-FACT 1)
(fact (+ 2 3))

