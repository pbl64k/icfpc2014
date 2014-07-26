; binary tree for fast map lookups?
; try to eat the friggin' fruit?
; efficient queues?
; BFS?
; all-sources shortest paths in main?
; take into account the number of ghosts on the field when scoring
; use the information about ghosts' direction somehow?
; reinforcement learning? (yeah, right.)
; with-matrix or somesuch would help? -- prolly not a good idea, no way to index sanely
(def main
    (fun [ws]
        (let* (
            [wmap (ws-map ws)]
            [h (length wmap)]
            [w (length (car wmap))]
            [ps (cart w h)]
            [fs (filter (fun [p] (> (cell-score (m-ix wmap p)) 0)) ps)]
            )
            (cons (ai-cons 0 fs) (fun-abi [a b] (step a b))))))
(def step
    (fun [ai-state world-state]
        (let* (
            [wmap (ws-map world-state)]
            [loc (lm-loc (ws-lmst world-state))]
            [move-cells (map (fun [d] (vec-+ d loc)) neighbors)]
            [valid-cells (filter (fun [pos] (valid-cell? wmap pos)) move-cells)]
            [cell-costs (map-map (fun [x] (ai-score ai-state world-state x)) valid-cells)]
            [best-cost (pick-best cell-costs)]
            [best-cell (car best-cost)]
            [nb-movs (nb-moves)]
            [match (filter (fun [mv] (vec-=? (vec-+ (cdr mv) loc) best-cell)) nb-movs)]
            ;[best-move (do (debug cell-costs) (debug best-cost) (debug match) (car (car match)))]
            [best-move (car (car match))]
            )
            (cons (ai-drop-food (ai-add-cell ai-state best-cell) best-cell) best-move))))
(def ai-score
    (fun [ai ws pos]
        (let* (
            [rec-cells (ai-rct ai)]
            [rec-sc (* -2 (rec-score rec-cells pos))]
            [wmap (ws-map ws)]
            [ghosts (ws-ghst ws)]
            [cell (m-ix wmap pos)]
            [csc (cell-score cell)]
            [ghsc (sum (map (fun [ghost] (ghost-score ws ghost pos)) ghosts))]
            [tgt-food (nearest-food ai ws)]
            [fdsc (* -5 (vec-l1-dist tgt-food pos))]
            )
            (+ rec-sc (+ fdsc (+ ghsc csc))))))
(def rec-score
    (fun [cells pos]
        (length (filter (fun [x] (vec-=? x pos)) cells))))
(def ghost-score
    (fun [ws gh pos]
        (let* (
            [gloc (gh-loc gh)]
            [dist (ghost-dist pos gloc)]
            [factor (ghost-factor (gh-vit gh))]
            )
            (* factor dist))))
(def ghost-factor
    (fun [v]
        (if [= v GH-STD]
            5
            (if [= v GH-FEAR]
                -1
                0))))
(def ghost-dist
    (fun [lm gh]
        (let (
            [d (vec-l1-dist lm gh)]
            )
            (if [<= d 1] ; DEATH IMMINENT
                -9000
                (if [< d GHOST-PROXIMITY-THRESHOLD]
                    d
                    GHOST-PROXIMITY-THRESHOLD)))))
(def nearest-food
    (fun [ai ws]
        (let* (
            [loc (lm-loc (ws-lmst ws))]
            [fs (ai-food ai)]
            [ffs (map-map (fun [p] (- 0 (vec-l1-dist loc p))) fs)]
            )
            (car (pick-best ffs)))))
(def cons-bst-map
    (fun [wmap]
        (let* (
            [h (length wmap)]
            [w (length (car wmap))]
            [sz (* h w)]
            [fmap (concat wmap)]
            )
            (cons (cons-bst fmap sz) (cons w (cons h 0))))))
(def cons-bst
    (fun [xs n]
        (if [= n 1]
            (cons 1 (car xs))
            (let* (
                [midp (/ n 2)]
                [sp (span midp xs)]
                )
                (cons 0 (cons midp (cons (cons-bst (car sp) midp) (cons (cons-bst (cdr sp) (- n midp)) 0))))))))
(def bstm-map (fun [bst] (ith 0 bst)))
(def bstm-w (fun [bst] (ith 1 bst)))
(def bstm-h (fun [bst] (ith 2 bst)))
(def bstm-ix
    (fun [wmap pos]
        (bstm-fix (bstm-map wmap) (+ (car pos) (* (cdr pos) (bstm-w wmap))))))
(def bstm-fix
    (fun [fmap n]
        (if [car fmap]
            (cdr fmap)
            (if [< n (car (cdr fmap))]
                (recur (car (cdr (cdr fmap))) n)
                (recur (car (cdr (cdr (cdr fmap)))) (- n (car (cdr fmap))))))))
(def ai-cons
    (fun [recent-cells food]
        (cons recent-cells (cons food 0))))
(def ai-add-cell
    (fun [ai cell]
        (let* (
            [cells (ai-rct ai)]
            [trimmed (take 50 cells)]
            [new-cells (cons cell trimmed)]
            )
            (ai-cons new-cells (ai-food ai)))))
(def ai-drop-food
    (fun [ai cell]
        (let* (
            [food (ai-food ai)]
            [new-food (filter (fun [x] (not (vec-=? x cell))) food)]
            )
            (ai-cons (ai-rct ai) new-food))))
(def pick-best
    (fun [xs]
        (pick-best-acc (car xs) (cdr xs))))
(def pick-best-acc
    (fun [acc xs]
        (if [atom? xs]
            acc
            (if [> (cdr (car xs)) (cdr acc)]
                (pick-best-acc (car xs) (cdr xs))
                (pick-best-acc acc (cdr xs))))))
(def cart (fun [x y] (cart-acc 0 0 0 x y)))
(def cart-acc
    (fun [acc x y w h]
        (if [= x w]
            (if [= y h]
                acc
                (recur acc 0 (+ y 1) w h))
            (recur (cons (cons x y) acc) (+ x 1) y w h))))
(def foldl
    (fun [f init xs]
        (if [atom? xs]
            init
            (recur f (f init (car xs)) (cdr xs)))))
(def map-map (fun [f xs] (map (fun [x] (cons x (f x))) xs)))
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
(def span
    (fun [n xs]
        (let (
            [acc (span-acc n (cons NIL NIL) xs)]
            )
            (cons (reverse (car acc)) (cdr acc)))))
(def span-acc
    (fun [n acc xs]
        (if [= n 0]
            (cons (car acc) xs)
            (recur (- n 1) (cons (cons (car xs) (car acc)) (cdr acc)) (cdr xs)))))
(def reverse (fun [xs] (rev-acc NIL xs)))
(def rev-acc
    (fun [acc xs]
        (if [atom? xs]
            acc
            (recur (cons (car xs) acc) (cdr xs)))))
(def concat (fun [xs] (reverse (concat-acc NIL xs))))
(def concat-acc
    (fun [acc xs]
        (if [atom? xs]
            acc
            (if [atom? (car xs)]
                (recur acc (cdr xs))
                (recur (cons (car (car xs)) acc) (cons (cdr (car xs)) (cdr xs)))))))
; non-tail recursive!
(def take
    (fun [n xs]
        (if [= n 0]
            0
            (if [atom? xs]
                0
                (cons (car xs) (take (- n 1) (cdr xs)))))))
; TODO? I'm not sure this makes sense if the last element of a "tuple" is a cons cell itself
(def ith
    (fun [ix xs]
        (if [atom? xs]
            xs
            (if ix
                (recur (- ix 1) (cdr xs))
                (car xs)))))
(def length (fun [xs] (foldl (fun [a x] (+ a 1)) 0 xs)))
(def sum (fun [xs] (foldl (fun [a b] (+ a b)) 0 xs)))
(def prod (fun [xs] (foldl (fun [a b] (* a b)) 0 xs)))
(def vec-+
    (fun [a b]
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
(def vec-=?
    (fun [a b]
        (if [= (car a) (car b)]
            (if [= (cdr a) (cdr b)]
                1
                0)
            0)))
(def not (fun [x] (- 1 x)))
(def abs
    (fun [x]
        (if [< x 0]
            (- 0 x)
            x)))
(def vec-l1-dist
    (fun [a b]
        (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b))))))
(def m-ix
    (fun [wmap pos]
        (if [< (car pos) 0]
            0
            (if [< (cdr pos) 0]
                0
                (ith (car pos) (ith (cdr pos) wmap))))))
(def valid-cell?
    (fun [wmap pos]
        (let (
            [st (m-ix wmap pos)]
            )
            (> st M-WALL))))
(def cell-score
    (fun [cell]
        (if [= cell M-PILL]
            10
            (if [= cell M-PPILL]
                50
                0))))
(def NIL 0)
(def neighbors (cons (cons 1 0) (cons (cons -1 0) (cons (cons 0 1) (cons (cons 0 -1) 0)))))
(def nb-moves (fun [] (zip (cons DIR-RT (cons DIR-LT (cons DIR-DN (cons DIR-UP NIL)))) neighbors)))
(def M-WALL 0)
(def M-EMPTY 1)
(def M-PILL 2)
(def M-PPILL 3)
(def M-FRUIT 4)
(def M-LMSP 5)
(def M-GHSP 6)
(def GH-STD 0)
(def GH-FEAR 1)
(def GH-INV 2)
(def DIR-UP 0)
(def DIR-RT 1)
(def DIR-DN 2)
(def DIR-LT 3)
; tried 5, 6 and 8
(def GHOST-PROXIMITY-THRESHOLD 8)
(def ai-rct (fun [ai] (ith 0 ai)))
(def ai-food (fun [ai] (ith 1 ai)))
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
(do
    ;(debug (step (ai-cons 0) (cons test-map (cons test-lms (cons test-ghs test-frs)))))
    ;(!0 1) retrieves the zeroth argument passed by external caller
    (main (!0 1)))

