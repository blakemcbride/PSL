(de fac (n)
    (cond ((eq n 0) 1) 
          (t (times n (fac (difference n 1))))
    )
)
(de ttt nil (progn
(print (setq l (fac 15)))
(print (setq i (times l 77)))
(quotient i l) ))

