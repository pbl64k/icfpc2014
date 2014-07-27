; BFS?
; exclusion zones around ghosts?
; connectivity?
; go for pills if ghosts nearby? (check that there ARE pills)
; use the information about ghosts' direction somehow?
; take into account the number of ghosts on the field when scoring
; try to eat the friggin' fruit?
; all-sources shortest paths in main?
; reinforcement learning? (yeah, right.)
; with-matrix or somesuch would help? -- prolly not a good idea, no way to index sanely

;;; EXTERNAL INTERFACE

; implements the logic of standard `main' (but is not the actual entry point)
(def main
    (fun [ws]
        (let* (
            [wmap (ws-map ws)]
            [bstmap (bstm-cons wmap)]
            [h (length wmap)]
            [w (length (car wmap))]
            [ps (cart w h)]
            [fs (filter (fun [p] (> (cell-score (bstm-ix bstmap p)) 0)) ps)]
            )
            (cons (ai-cons NIL fs) (fun-abi [a b] (bfs-ai a b))))))

; implements the logic of standard `step' -- but `main` must ensure it's converted to fun-abi
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

; alternate `step'
; heh heh! fall back to old step if this fails.
(def bfs-ai
    (fun [ai-state ws]
        (let* (
            [wmap (bstm-cons (ws-map ws))]
            [my-loc (lm-loc (ws-lmst ws))]
            [my-floc (bstm-flatten-ix wmap my-loc)]
            [f-neighbors (lm-neighbors-gen wmap (lm-valid-cell?-gen wmap ws))]
            [init-moves (f-neighbors my-loc -1)]
            [init-frontier (foldl q-snoc q-empty init-moves)]
            [init-visited (set-ins set-empty my-floc)]
            [best-move (bfs wmap f-neighbors (fun [p] (> (cell-score (bstm-ix wmap p)) 0)) init-frontier init-visited)]
            ;[best-move FALSE]
            )
            (do
                ;(debug my-loc)
                ;(debug init-moves)
                ;(debug best-move)
                (if best-move
                    (cons (ai-drop-food (ai-add-cell ai-state (cdr best-move)) (cdr best-move)) (car best-move))
                    (step ai-state ws))))))

;;; SCORING AND AI

(def lm-neighbors-gen
    (fun [wmap f-valid-cell?] ; doesn't really need `wmap' as is
        (fun [pos preset-dir]
            (let* (
                [nb-movs (nb-moves)]
                [nb-ps (map (fun [mov] (cons (car mov) (vec-+ pos (cdr mov)))) nb-movs)]
                [nb-dps (map (fun [mov] (if [< preset-dir 0] mov (cons preset-dir (cdr mov)))) nb-ps)]
                ; now we have originating directions and actual positions...
                )
                (filter (fun [mov] (f-valid-cell? (cdr mov))) nb-dps)))))

(def lm-valid-cell?-gen
    (fun [wmap ws]
        (fun [pos]
            (if [> (bstm-ix wmap pos) M-WALL]
                (not (any? (fun [gh-loc] (vec-=? pos gh-loc)) (map gh-loc (filter (fun [gh] (not (gh-fear? (gh-vit gh)))) (ws-ghst ws)))))
                FALSE))))

(def bfs
    (fun [bstm-w f-neighbors f-tgt? q-frontier set-visited]
        ; dodgy stuff here! will if's and let's work like that?
        (do
            (debug q-frontier)
            (debug set-visited)
            (if [q-isempty? q-frontier]
                FALSE
                (let* (
                    [state-1 (q-pop q-frontier)]
                    [mov (car state-1)]
                    [q-frontier-1 (cdr state-1)]
                    [m-dir (car mov)]
                    [m-pos (cdr mov)]
                    [m-fpos (bstm-flatten-ix bstm-w m-pos)]
                    )
                    (if [set-has? set-visited m-fpos]
                        (recur bstm-w f-neighbors f-tgt? q-frontier-1 set-visited)
                        (if [f-tgt? m-pos]
                            mov
                            (let* (
                                [faux 0]
                                ;[set-visited-2 (set-ins set-visited m-fpos)]
                                ;[new-moves (filter (fun [x] (not (set-has? set-visited-2 (bstm-flatten-ix bstm-w (cdr x))))) (f-neighbors m-pos m-dir))]
                                ;[q-frontier-2 (foldl q-snoc q-frontier-1 new-moves)]
                                )
                                FALSE))))))))
                                ;(recur bstm-w f-neighbors f-tgt? q-frontier-2 set-visited-2)))))))))
                                ;(recur bstm-w f-neighbors f-tgt? q-frontier-1 set-visited)))))))))

; helpers for old AI (`step') follow

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
; tried 5, 6 and 8
; 8 looks best after all, but the whole threshold thing is unsatisfactory
(def GHOST-PROXIMITY-THRESHOLD 8)
(def ghost-dist
    (fun [lm gh]
        (let (
            [d (vec-l1-dist lm gh)]
            )
            (if [<= d 2] ; DEATH IMMINENT (note that death may still be imminent at 2 -- but weird behavior results)
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
; map access (in standard crummy representation)
(def m-ix
    (fun [wmap pos]
        (if [< (car pos) 0]
            M-WALL
            (if [< (cdr pos) 0]
                M-WALL
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

;;; functions for operating on the ai state

(def ai-cons
    (fun [recent-cells food]
        (cons recent-cells (cons food NIL))))
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
(def ai-rct (fun [ai] (ith 0 ai)))
(def ai-food (fun [ai] (ith 1 ai)))

;;; BST-based representation of game map

(def bstm-cons
    (fun [wmap]
        (let* (
            [h (length wmap)]
            [w (length (car wmap))]
            [sz (* h w)]
            [fmap (concat wmap)]
            )
            (cons (bst-cons fmap sz) (cons w (cons h NIL))))))
(def bst-cons
    (fun [xs n]
        (if [= n 1]
            (cons 1 (car xs))
            (let* (
                [midp (/ n 2)]
                [sp (span midp xs)]
                )
                (cons 0 (cons midp (cons (bst-cons (car sp) midp) (cons (bst-cons (cdr sp) (- n midp)) NIL))))))))
(def bstm-map (fun [bst] (ith 0 bst)))
(def bstm-w (fun [bst] (ith 1 bst)))
(def bstm-h (fun [bst] (ith 2 bst)))
(def bstm-flatten-ix
    (fun [wmap pos]
        (+ (car pos) (* (cdr pos) (bstm-w wmap)))))
(def bstm-ix
    (fun [wmap pos]
        (bstm-fix (bstm-map wmap) (bstm-flatten-ix wmap pos))))
(def bstm-fix
    (fun [fmap n]
        (if [car fmap]
            (cdr fmap)
            (if [< n (car (cdr fmap))]
                (recur (car (cdr (cdr fmap))) n)
                (recur (car (cdr (cdr (cdr fmap)))) (- n (car (cdr fmap))))))))

;;; SIMPLE PURELY FUNCTIONAL QUEUES

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

;;; INTEGER TREESETS

(def set-empty 0)
(def set-ins
    (fun [s x]
        (if [atom? s]
            (cons x (cons NIL NIL))
            (if [= x (car s)]
                s
                (if [< x (car s)]
                    (cons (car s) (cons (set-ins (car (cdr s)) x) (cdr (cdr s))))
                    (cons (car s) (cons (car (cdr s)) (set-ins (cdr (cdr s)) x))))))))
(def set-has?
    (fun [s x]
        (if [atom? s]
            FALSE
            (if [= x (car s)]
                TRUE
                (if [< x (car s)]
                    (recur (car (cdr s)) x)
                    (recur (cdr (cdr s)) x))))))

;;; MISC

; accepts a list of pairs
; PRECOND: list non-empty
; PRECOND: elements are conses with cdrs comparable by >
; return a pair with the highest cdr
(def pick-best
    (fun [xs]
        (pick-best-acc (car xs) (cdr xs))))
(def pick-best-acc
    (fun [acc xs]
        (if [atom? xs]
            acc
            (if [> (cdr (car xs)) (cdr acc)]
                (recur (car xs) (cdr xs))
                (recur acc (cdr xs))))))

; accepts two numbers
; returns a cartesian product of [0 .. x - 1] and [0 .. y - 1]
(def cart (fun [x y] (cart-acc 0 0 0 x y)))
(def cart-acc
    (fun [acc x y w h]
        (if [= x w]
            (if [= y h]
                acc
                (recur acc 0 (+ y 1) w h))
            (recur (cons (cons x y) acc) (+ x 1) y w h))))

;;; LIST FUNCTIONS

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
(def any?
    (fun [f xs]
        (if [atom? xs]
            FALSE
            (if [f (car xs)]
                TRUE
                (recur f (cdr xs))))))
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
(def take
    (fun [n xs]
        (if [= n 0]
            NIL
            (if [atom? xs]
                NIL
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

;;; VECTOR FUNCTIONS
;;; vectors are pairs of integers
;;; does not use loc-foo to save a few cycles

(def vec-+
    (fun [a b]
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
(def vec-=?
    (fun [a b]
        (if [= (car a) (car b)]
            (if [= (cdr a) (cdr b)]
                TRUE
                FALSE)
            FALSE)))
(def vec-l1-dist
    (fun [a b]
        (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b))))))

;;; MISC MATH AND LOGIC

(def not (fun [x] (- 1 x)))
(def abs
    (fun [x]
        (if [< x 0]
            (- 0 x)
            x)))

;;; CONSTANTS AND SUCH
;;; note that constants may not be used to compute other constants, as toplevel is `let', not a `let*'
;;; this ensures recursive and mutually recursive functions can be defined at the toplevel,
;;; but limits the use of constants in the outer frame

(def NIL 0)
(def TRUE 1)
(def FALSE 0)
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

;;; ACCESSORS AND INSPECTORS FOR STANDARD DATA STRUCTURES

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
; note that `spec' functions are not first-class, so can't just alias
(def loc-x (fun [loc] (car loc)))
(def loc-y (fun [loc] (cdr loc)))

;;; STUFF FOR TESTING

(def test-map (cons (cons 0 (cons 1 (cons 0 0))) (cons (cons 0 (cons 1 (cons 2 0))) 0)))
(def test-lms (cons 0 (cons (cons 1 1) (cons 0 (cons 3 100)))))
(def test-ghs (cons (cons 0 (cons (cons 1 2) 3)) 0))
(def test-frs 0)

;;; ENTRY POINT
;;; grabs the arguments passed from the outside and calls the actual implementation

(do
    ;(debug (step (ai-cons 0) (cons test-map (cons test-lms (cons test-ghs test-frs)))))
    ;(!0 1) retrieves the zeroth argument passed by external caller
    (main (!0 1)))

