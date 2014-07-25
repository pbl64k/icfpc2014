(def main
    (fun []
        (cons 0 step)))
(def step
    (fun [ai-state world-state]
        (cons ai-state 1)))
(main)
