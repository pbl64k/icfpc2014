(def main
    (fun [arg]
        (cons arg (fun-abi [a b] (step a b)))))
(def step
    (fun [ai-state world-state]
        (let* (
            [wmap (ws-map world-state)]
            [loc (lm-loc (ws-lmst world-state))]
            [move-cells (map (fun [d] (vec-add d (cons 0 0))) neighbors)]
            [valid-cells (filter (fun [pos] (valid-cell? wmap pos)) move-cells)]
            [stupid-cell (do (debug loc) (debug move-cells) (debug valid-cells) (car valid-cells))]
            )
            (cons ai-state 3))))
(def map (fun [f xs] (reverse (map-rev NIL f xs))))
(def map-rev
    (fun [acc f xs]
        (if [atom? xs]
            acc
            (recur (cons (f (car xs)) acc) f (cdr xs)))))
(def filter (fun [f xs] (reverse (filter-rev NIL f xs))))
(def filter-rev
    (fun [acc f xs]
        (if [atom? xs]
            acc
            (if [f (car xs)]
                (recur (cons (car xs) acc) f (cdr xs))
                (recur acc f (cdr xs))))))
(def zip (fun [a b] (reverse (zip-rev NIL a b))))
(def zip-rev
    (fun [acc a b]
        (if [atom? a]
            acc
            (if [atom? b]
                acc
                (recur (cons (cons (car a) (car b)) acc) (cdr a) (cdr b))))))
(def reverse (fun [xs] (rev-acc NIL xs)))
(def rev-acc
    (fun [acc xs]
        (if [atom? xs]
            acc
            (recur (cons (car xs) acc) (cdr xs)))))
(def ith
    (fun [ix xs]
        (if [atom? xs]
            xs
            (if ix
                (recur (- ix 1) (cdr xs))
                (car xs)))))
(def vec-add
    (fun [a b]
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
(def vec-=?
    (fun [a b]
        (if [= (car a) (car b)]
            (if [= (cdr a) (cdr b)]
                1
                0)
            0)))
(def m-ix
    (fun [wmap pos]
        (if [< (car pos) 0]
            0
            (if [< (cdr pos) 0]
                0
                (ith (cdr pos) (ith (car pos) wmap))))))
(def valid-cell?
    (fun [wmap pos]
        (let (
            [st (m-ix wmap pos)]
            )
            (> st M-WALL))))
(def NIL 0)
(def neighbors (cons (cons 1 0) (cons (cons -1 0) (cons (cons 0 1) (cons (cons 0 -1) 0)))))
(def nb-moves (fun [] (zip (cons DIR-DN (cons DIR-UP (cons DIR-RT (cons DIR-LT NIL)))) neighbors)))
(def M-WALL 0)
(def M-EMPTY 1)
(def M-PILL 2)
(def M-PPILL 3)
(def M-FRUIT 4)
(def M-LMSP 5)
(def M-GHSP 6)
(def GH-STD 0)
(def GH-FEAR 1)
(def DIR-UP 0)
(def DIR-RT 1)
(def DIR-DN 2)
(def DIR-LT 3)
(def GH-INV 2)
(def ws-map (fun [ws] (ith 0 ws)))
(def ws-lmst (fun [ws] (ith 1 ws)))
(def ws-ghst (fun [ws] (ith 2 ws)))
(def ws-fruit (fun [ws] (ith 3 ws)))
(def m-wall? (fun [x] (= x M-WALL)))
(def m-empty? (fun [x] (= x M-EMPTY)))
(def m-pill? (fun [x] (= x M-PILL)))
(def m-ppill? (fun [x] (= x M-PPILL)))
(def m-floc? (fun [x] (= x M-FRUIT)))
(def m-lmsp? (fun [x] (= x M-LMSP)))
(def m-ghsp? (fun [x] (= x M-GHSP)))
(def lm-vit (fun [lm] (ith 0 lm)))
(def lm-loc (fun [lm] (ith 1 lm)))
(def lm-dir (fun [lm] (ith 2 lm)))
(def lm-lives (fun [lm] (ith 3 lm)))
(def lm-score (fun [lm] (ith 4 lm)))
(def gh-vit (fun [gh] (ith 0 gh)))
(def gh-loc (fun [gh] (ith 1 gh)))
(def gh-dir (fun [gh] (ith 2 gh)))
(def gh-std? (fun [vit] (= vit GH-STD)))
(def gh-fear? (fun [vit] (= vit GH-FEAR)))
(def gh-inv? (fun [vit] (= vit GH-INV)))
(def loc-x (fun [loc] (car loc)))
(def loc-y (fun [loc] (cdr loc)))
(def test-map (cons (cons 0 (cons 1 (cons 0 0))) (cons (cons 0 (cons 1 (cons 2 0))) 0)))
(def test-lms (cons 0 (cons (cons 1 1) (cons 0 (cons 3 100)))))
(def test-ghs (cons (cons 0 (cons (cons 1 2) 3)) 0))
(def test-frs 0)
(main (step 0 (cons test-map (cons test-lms (cons test-ghs test-frs)))))

