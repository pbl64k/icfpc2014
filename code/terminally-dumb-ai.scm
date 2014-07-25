(def main
    (fun []
        (cons 0 step)))
(def step
    (fun [ai-state world-state]
        (cons ai-state DIR-LT)))
(def ith
    (fun [ix xs]
        (if [atom? xs]
            xs
            (if ix
                (recur (- ix 1) (cdr xs))
                (car xs)))))
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
(main)
