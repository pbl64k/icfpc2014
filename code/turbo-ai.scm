; ! Consider ALL moves -- and pick the best one that finds something edible.
; ! (Fall back) limit the number of steps and score by distance to nearest if too far away?
; ! Optimize heavy-duty stuff? (bfs)
; + go for pills if ghosts nearby? (check that there ARE pills)
; ? dangerous being near ghost spawn points?
; ? figured it out: old AI may return no moves, too -- TagMismatch on classic when munching on tons of ghosts, omg?
; ? optimize list functions? -- not necessarily such a good idea (not clear which approach is better in practice)
; - only relevant for the idiotic AI -- take into account the number of ghosts on the field when scoring (don't bother.)
; - connectivity? (duh.)
; - all-sources shortest paths in main? (meeh.)
; - reinforcement learning? (yeah, right.)
; - with-matrix or somesuch would help? -- prolly not a good idea, no way to index sanely

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
            [fff (fun [x] (if [atom? x] (cons -1 -1) (car x)))]
            [fruit-loc (fff (filter (fun [p] (m-floc? (bstm-ix bstmap p))) ps))]
            [ppills (filter (fun [p] (m-ppill? (bstm-ix bstmap p))) ps)]
            )
            (cons (ai-cons NIL fs fruit-loc ppills) (fun-abi [a b] (bfs-ai a b))))))

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
            [match (filter (fun [mv] (vec-=? (vec-+ (cdr mv) loc) best-cell)) nb-moves)]
            [best-move (car (car match))]
            )
            (do
                (debug 0)
                (let (
                    [new-ai (ai-update ai-state best-cell)]
                    )
                    (do
                        ;(debug 1)
                        (cons new-ai best-move)))))))

; alternate `step'
; heh heh! fall back to old step if this fails.
(def bfs-ai
    (fun [ai-state ws]
        (let* (
            [wmap (bstm-cons (ws-map ws))]
            [my-loc (lm-loc (ws-lmst ws))]
            [my-floc (bstm-flatten-ix wmap my-loc)]
            [f-neighbors (lm-neighbors-gen wmap (lm-valid-cell?-gen wmap ws))]
            [f-cell-score (lm-cell-score-gen wmap ws ai-state)]
            [ghosties (ws-ghst ws)]
            [bad-ghosties (filter (fun [gh] (gh-std? (gh-vit gh))) ghosties)]
            [gh-pairs (map (fun [gh] (cons (gh-dir gh) (cons (gh-loc gh) NIL))) bad-ghosties)]
            [init-moves (f-neighbors -1 my-loc 1 gh-pairs 0)]
            ;[init-moves (f-neighbors -1 my-loc 1 NIL)]
            [init-frontier (q-append q-empty init-moves)]
            [init-visited (set-ins set-empty my-floc)]
            [nrst-food (nearest-food ai-state ws)]
            [nrst-ppill (nearest-ppill ai-state ws)]
            [best-moves (turbo-bfs NIL wmap f-neighbors (fun [p extra-bad-ghosties] (> (f-cell-score p extra-bad-ghosties) 0)) init-frontier init-visited nrst-food nrst-ppill)]
            ;[best-move (bfs wmap f-neighbors (fun [p] (> (cell-score (bstm-ix wmap p)) 0)) init-frontier init-visited)]
            ;[best-move FALSE]
            )
            (do
                (if [not (atom? best-moves)]
                    (let* (
                        [best-move (bfs-best (car best-moves) (cdr best-moves))]
                        )
                        (cons (ai-update ai-state (bfsx-pos best-move)) (bfsx-dir best-move)))
                    (step ai-state ws))))))

;;; SCORING AND AI

(def lm-neighbors-gen
    (fun [wmap f-valid-cell?]
        (fun [preset-dir pos dist ghs sc]
            (let* (
                [new-ghosts (map (fun [gh] (move-ghost wmap gh)) ghs)]
                )
                (filter
                    (fun [mov] (f-valid-cell? (bfsx-pos mov) (append ghs new-ghosts)))
                    (map
                        (fun [mov]
                            (if [< preset-dir 0]
                                mov
                                (bfsx-ch-dir mov preset-dir)))
                        (map
                            (fun [mov]
                                ; right now the score is always 0 - this is for future extension
                                (bfsx-cons (car mov) (vec-+ pos (cdr mov)) dist new-ghosts sc))
                            nb-moves)))))))

(def lm-valid-cell?-gen
    (fun [wmap ws]
        (let (
            ; I'm not sure ignoring invisible ghosts is a good idea
            [ghosties (map gh-loc (filter (fun [gh] (gh-std? (gh-vit gh))) (ws-ghst ws)))]
            )
            (fun [pos extra-bad-ghosties]
                (if [> (bstm-ix wmap pos) M-WALL]
                    (not (any? (fun [gh-loc] (<= (vec-l1-dist pos gh-loc) 1)) (append ghosties (map gh-loc extra-bad-ghosties))))
                    FALSE)))))

(def lm-cell-score-gen
    (fun [wmap ws ai]
        ; well enough optimized - note that the precomputed stuff is used by inner functions
        (let* (
            [ghosties (ws-ghst ws)]
            ; I'm not sure ignoring invisible ghosts is a good idea
            [bad-ghosties (filter (fun [gh] (gh-std? (gh-vit gh))) ghosties)]
            [good-ghosties (filter (fun [gh] (gh-fear? (gh-vit gh))) ghosties)]
            [lm-vit-score (lm-vit (ws-lmst ws))]
            [fruit (ws-fruit ws)]
            [fruit-loc (ai-fruit ai)]
            [lm-good-ghost-score
                (fun [gh pos]
                    (if [vec-=? (gh-loc gh) pos] ; ignore the nonsense, we'll not account for expiring POWER
                        (* 50 lm-vit-score) ; !!! change this to account for the number of moves taken
                        0))]
            [lm-bad-ghost-score
                (fun [gh pos]
                    (let (
                        [d (vec-l1-dist (gh-loc gh) pos)]
                        )
                        (if [= d 0]
                            -9000
                            (if [= d 1]
                                -9000
                                (if [= d 2]
                                    -10
                                    0)))))]
            [lm-ghost-score
                (fun [pos extra-bad-ghosties]
                    (+ (sum (map (fun [gh] (lm-bad-ghost-score gh pos)) bad-ghosties)) (+ (sum (map (fun [gh] (lm-bad-ghost-score gh pos)) extra-bad-ghosties)) (sum (map (fun [gh] (lm-good-ghost-score gh pos)) good-ghosties)))))]
            [lm-fruit-score
                (fun [pos]
                    (if [> fruit 0]
                        (if [vec-=? pos fruit-loc]
                            (* fruit 20) ; !!! change this to account for the number of moves taken
                            0)
                        0))]
            )
            (fun [pos extra-bad-ghosties]
                (+ (* 10 (cell-score (bstm-ix wmap pos))) (+ (lm-ghost-score pos extra-bad-ghosties) (lm-fruit-score pos)))))))

(def TURBO-THRESH 20)

; nothing particularly fast about it
; it's just meant to be TURBO.
(def turbo-bfs
    ; !!! change f-tgt? to value function
    (fun [acc bstm-w f-neighbors f-tgt? q-frontier set-visited nrst-food nrst-ppill]
        (do
            (if [q-isempty? q-frontier]
                acc
                ; !!! optimization
                (let* (
                    [state-1 (q-pop q-frontier)]
                    [mov (car state-1)]
                    [q-frontier-1 (cdr state-1)]
                    [m-dir (bfsx-dir mov)]
                    [m-pos (bfsx-pos mov)]
                    [m-dist (bfsx-dist mov)]
                    [m-ghs (bfsx-ghs mov)]
                    [m-fpos (bstm-flatten-ix bstm-w m-pos)]
                    )
                    (if [set-has? set-visited m-fpos]
                        (recur acc bstm-w f-neighbors f-tgt? q-frontier-1 set-visited nrst-food nrst-ppill)
                        ; !!! recompute the score HERE -- need m-sc (NEW)
                        ; !!! optimization
                        (let* (
                            [m-sc (bfsx-sc mov)]
                            )
                            (if [f-tgt? m-pos m-ghs]
                                (recur (bfs-ins-move acc mov) bstm-w f-neighbors f-tgt? q-frontier-1 (set-ins set-visited m-fpos) nrst-food nrst-ppill)
                                ; !!! add a branch for expired moves
                                ; !!! optimization
                                (let* (
                                    [set-visited-2 (set-ins set-visited m-fpos)]
                                    [m-neighbors (f-neighbors m-dir m-pos (+ 1 m-dist) m-ghs m-sc)]
                                    [new-moves (filter (fun [x] (not (set-has? set-visited-2 (bstm-flatten-ix bstm-w (bfsx-pos x))))) m-neighbors)]
                                    [q-frontier-2 (q-append q-frontier-1 new-moves)]
                                    )
                                    (recur acc bstm-w f-neighbors f-tgt? q-frontier-2 set-visited-2 nrst-food nrst-ppill))))))))))

(def bfs-ins-move
    (fun [acc mov]
        (if [atom? acc]
            (cons mov NIL)
            (if [= (bfsx-dir (car acc)) (bfsx-dir mov)]
                (if [> (bfsx-sc mov) (bfsx-sc (car acc))]
                    (cons mov (cdr acc))
                    acc)
                (cons (car acc) (bfs-ins-move (cdr acc) mov))))))

(def bfs-best
    (fun [acc xs]
        (if [atom? xs]
            acc
            (if [> (bfsx-sc (car xs)) (bfsx-sc acc)]
                (recur (car xs) (cdr xs))
                (recur acc (cdr xs))))))

; !!! remove it later
(def non-turbo-bfs
    (fun [bstm-w f-neighbors f-tgt? q-frontier set-visited nrst-food nrst-ppill]
        (do
            (if [q-isempty? q-frontier]
                FALSE
                ; !!! optimization
                (let* (
                    [state-1 (q-pop q-frontier)]
                    [mov (car state-1)]
                    [q-frontier-1 (cdr state-1)]
                    [m-dir (bfsx-dir mov)]
                    [m-pos (bfsx-pos mov)]
                    [m-dist (bfsx-dist mov)]
                    [m-ghs (bfsx-ghs mov)]
                    [m-sc (bfsx-sc mov)]
                    [m-fpos (bstm-flatten-ix bstm-w m-pos)]
                    )
                    (if [set-has? set-visited m-fpos]
                        (recur bstm-w f-neighbors f-tgt? q-frontier-1 set-visited nrst-food nrst-ppill)
                        (if [f-tgt? m-pos m-ghs]
                            mov
                            ; !!! optimization
                            (let* (
                                ; !!! recompute the score
                                [set-visited-2 (set-ins set-visited m-fpos)]
                                [m-neighbors (f-neighbors m-dir m-pos (+ 1 m-dist) m-ghs m-sc)]
                                [new-moves (filter (fun [x] (not (set-has? set-visited-2 (bstm-flatten-ix bstm-w (bfsx-pos x))))) m-neighbors)]
                                [q-frontier-2 (q-append q-frontier-1 new-moves)]
                                )
                                (recur bstm-w f-neighbors f-tgt? q-frontier-2 set-visited-2 nrst-food nrst-ppill)))))))))

(def bfsx-cons
    (fun [dir pos dist ghs sc]
        (cons dir (cons pos (cons dist (cons ghs sc))))))
(def bfsx-ch-dir (fun [x dir] (bfsx-cons dir (bfsx-pos x) (bfsx-dist x) (bfsx-ghs x) (bfsx-sc x))))
(def bfsx-add-sc (fun [x sc] (bfsx-cons (bfsx-dir x) (bfsx-pos x) (bfsx-dist x) (bfsx-ghs x) (+ sc (bfsx-sc x)))))
(def bfsx-dir (fun [x] (car x)))
(def bfsx-pos (fun [x] (car (cdr x))))
(def bfsx-dist (fun [x] (car (cdr (cdr x)))))
(def bfsx-ghs (fun [x] (car (cdr (cdr (cdr x))))))
(def bfsx-sc (fun [x] (cdr (cdr (cdr (cdr x))))))

;;; GHOST PSEUDO-AI

; accepts ghost as a LIST (direction, loc) -- (COINCIDENTALLY, this will work with gh-loc, as needed by lm-bad-ghost-score)
(def move-ghost
    (fun [wmap gh]
        (let* (
            [gh-pos (gh-loc gh)]
            [valid-cells (filter (fun [mov] (> (bstm-ix wmap (gh-loc mov)) M-WALL)) (map (fun [mov] (cons (car mov) (cons (vec-+ (cdr mov) gh-pos) NIL))) nb-moves))]
            [valid-cells-len (length valid-cells)]
            )
            (if [= valid-cells-len 1]
                (car valid-cells)
                (if [= valid-cells-len 2]
                    (car (filter (fun [cell] (if [= (car cell) (- (car gh) 2)] FALSE (if [= (car cell) (+ (car gh) 2)] FALSE TRUE))) valid-cells))
                    gh)))))

;;; helpers for old AI (`step') follow
;;; (some of them may be used in the "new" AI)

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
            (if [<= d 1] ; DEATH IMMINENT (note that death may still be imminent at 2 -- but weird behavior results)
                -9000
                (if [< d GHOST-PROXIMITY-THRESHOLD]
                    d
                    GHOST-PROXIMITY-THRESHOLD)))))
(def nearest-food
    (fun [ai ws]
        (let* (
            [loc (lm-loc (ws-lmst ws))]
            )
            (car (pick-best (map-map (fun [p] (- 0 (vec-l1-dist loc p))) (ai-food ai)))))))
(def nearest-ppill
    (fun [ai ws]
        (let* (
            [loc (lm-loc (ws-lmst ws))]
            [ppills (ai-ppills ai)]
            )
            (if [atom? ppills]
                NIL
                (car (pick-best (map-map (fun [p] (- 0 (vec-l1-dist loc p))) ppills)))))))
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
        (> (m-ix wmap pos) M-WALL)))
; DON'T TOUCH THIS! may screw up detection of pills at the start-up (and "old AI")
; reimplement if changes needed
(def cell-score
    (fun [cell]
        (if [= cell M-PILL]
            10
            (if [= cell M-PPILL]
                50
                0))))

;;; functions for operating on the ai state

(def ai-cons
    (fun [recent-cells food fruit-loc ppills]
        (cons recent-cells (cons food (cons fruit-loc ppills)))))
(def ai-add-cell
    (fun [ai cell]
        (ai-cons (cons cell (take 50 (ai-rct ai))) (ai-food ai) (ai-fruit ai) (ai-ppills ai))))
(def ai-drop-food
    (fun [ai cell]
        (ai-cons (ai-rct ai) (filter (fun [x] (not (vec-=? x cell))) (ai-food ai)) (ai-fruit ai) (ai-ppills ai))))
(def ai-drop-ppill
    (fun [ai cell]
        (ai-cons (ai-rct ai) (ai-food ai) (ai-fruit ai) (filter (fun [x] (not (vec-=? x cell))) (ai-ppills ai)))))
(def ai-update
    (fun [ai cell]
        (ai-drop-ppill (ai-drop-food (ai-add-cell ai cell) cell) cell)))
(def ai-rct (fun [ai] (car ai)))
(def ai-food (fun [ai] (car (cdr ai))))
(def ai-fruit (fun [ai] (car (cdr (cdr ai)))))
(def ai-ppills (fun [ai] (cdr (cdr (cdr ai)))))

;;; BST-based representation of game map

(def bstm-cons
    (fun [wmap]
        (let* (
            [h (length wmap)]
            [w (length (car wmap))]
            )
            (cons (bst-cons (concat wmap) (* h w)) (cons w h)))))
(def bst-cons
    (fun [xs n]
        (if [= n 1]
            (cons 1 (car xs))
            (let* (
                [midp (/ n 2)]
                [sp (span midp xs)]
                )
                (cons 0 (cons midp (cons (bst-cons (car sp) midp) (bst-cons (cdr sp) (- n midp)))))))))
(def bstm-map (fun [bst] (car bst)))
(def bstm-w (fun [bst] (car (cdr bst))))
(def bstm-h (fun [bst] (cdr (cdr bst))))
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
                (recur (cdr (cdr (cdr fmap))) (- n (car (cdr fmap))))))))

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
; this assumes that order doesn't matter
(def q-append
    (fun [q xs]
        (q-norm (cons (car q) (foldl (fun [r x] (cons x r)) (cdr q) xs)))))
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

; rewrite some in a non-tail recursive fashion? could save a few cycles

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
(def append
    (fun [xs ys]
        (if [atom? xs]
            ys
            (cons (car xs) (append (cdr xs) ys)))))
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
;(def nb-moves (fun [] (zip (cons DIR-RT (cons DIR-LT (cons DIR-DN (cons DIR-UP NIL)))) neighbors)))
(def nb-moves (cons (cons 1 (cons 1 0)) (cons (cons 3 (cons -1 0)) (cons (cons 2 (cons 0 1)) (cons (cons 0 (cons 0 -1)) 0)))))
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

(def ws-map (fun [ws] (car ws)))
(def ws-lmst (fun [ws] (car (cdr ws))))
(def ws-ghst (fun [ws] (car (cdr (cdr ws)))))
(def ws-fruit (fun [ws] (cdr (cdr (cdr ws)))))
(def m-wall? (fun [x] (= x M-WALL)))
(def m-empty? (fun [x] (= x M-EMPTY)))
(def m-pill? (fun [x] (= x M-PILL)))
(def m-ppill? (fun [x] (= x M-PPILL)))
(def m-floc? (fun [x] (= x M-FRUIT)))
(def m-lmsp? (fun [x] (= x M-LMSP)))
(def m-ghsp? (fun [x] (= x M-GHSP)))
(def lm-vit (fun [lm] (car lm)))
(def lm-loc (fun [lm] (car (cdr lm))))
(def lm-dir (fun [lm] (car (cdr (cdr lm)))))
(def lm-lives (fun [lm] (car (cdr (cdr (cdr lm))))))
(def lm-score (fun [lm] (cdr (cdr (cdr (cdr lm))))))
(def gh-vit (fun [gh] (car gh)))
(def gh-loc (fun [gh] (car (cdr gh))))
(def gh-dir (fun [gh] (cdr (cdr gh))))
(def gh-std? (fun [vit] (= vit GH-STD)))
(def gh-fear? (fun [vit] (= vit GH-FEAR)))
(def gh-inv? (fun [vit] (= vit GH-INV)))
; note that `spec' functions are not first-class, so can't just alias
(def loc-x (fun [loc] (car loc)))
(def loc-y (fun [loc] (cdr loc)))

;;; STUFF FOR TESTING

;(def test-map (cons (cons 0 (cons 1 (cons 0 0))) (cons (cons 0 (cons 1 (cons 2 0))) 0)))
;(def test-lms (cons 0 (cons (cons 1 1) (cons 0 (cons 3 100)))))
;(def test-ghs (cons (cons 0 (cons (cons 1 2) 3)) 0))
;(def test-frs 0)

;;; ENTRY POINT
;;; grabs the arguments passed from the outside and calls the actual implementation

(do
    ;(debug (step (ai-cons 0) (cons test-map (cons test-lms (cons test-ghs test-frs)))))
    ;(!0 1) retrieves the zeroth argument passed by external caller
    (main (!0 1)))

