(def NIL 0)

(def reverse (fun [xs] (rev-acc NIL xs)))
(def rev-acc
    (fun [acc xs]
        (if [atom? xs]
            acc
            (recur (cons (car xs) acc) (cdr xs)))))

(def q-empty (cons 0 0))
(def q-isempty?
    (fun [q] (atom? (car q))))
(def q-pop
    (fun [q]
        (cons (car (car q)) (q-norm (cons (cdr (car q)) (cdr q))))))
(def q-snoc
    (fun [q x]
        (q-norm (cons (car q) (cons x (cdr q))))))
(def q-norm
    (fun [q]
        (if [atom? (car q)]
            (cons (reverse (cdr q)) NIL)
            q)))

(let* (
    [q1 (q-snoc q-empty 1)]
    [q2 (q-snoc q1 2)]
    [q3 (q-snoc q2 3)]
    [p4 (q-pop q3)]
    [q5 (q-snoc (cdr p4) 4)]
    [q6 (q-snoc q5 5)]
    [p7 (q-pop q1)]
    [p8 (q-pop q2)]
    [p9 (q-pop q6)]
    [p10 (q-pop (cdr p9))]
    [p11 (q-pop (cdr p10))]
    )
    (do
        (debug q1)
        (debug q2)
        (debug q3)
        (debug p4)
        (debug q5)
        (debug q6)
        (debug p7)
        (debug p8)
        (debug p9)
        (debug p10)
        (debug p11)
        0))
