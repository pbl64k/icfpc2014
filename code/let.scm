(def f
    (fun []
        (let* (
            [a 1]
            [g (fun [x] (+ x 1))]
            [b (g a)]
            )
            (+ b 1))))
(f)
