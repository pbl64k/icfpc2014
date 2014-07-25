(def foldl (fun [f init xs]
    (if [atom? xs]
        init
        (foldl f (f init (car xs)) (cdr xs)))))
(def add (fun [a b] (+ a b)))
(foldl add 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 0))))))

