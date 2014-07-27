(def f
    (fun []
        (let* (
            [a 1]
            [b 2]
            [c (+ a b)]
            )
            (if [> c 2]
                1
                0))))

(f)
