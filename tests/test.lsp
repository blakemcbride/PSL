(de fac (n)
    (cond ((eq n 0) 1) 
          (t (times n (fac (difference n 1))))
    )
)

(de test!-bolu nil
  (prog (i j k l)
    (setq l 5)
l2  (prin1 l) (prin2 "-->") 
    (print (setq j (fac l)))
    (setq i 2)
l1  (cond ((eqn i 1300) (go l3)))
    (setq k (times j i))
    (cond ((not (eqn i (car (divide k j)))) (progn (prin1 i) (prin1 (divide k j)) (print "hata 1"))))
    (cond ((not (zerop (cdr (divide k j)))) (progn (prin1 i) (prin1 (divide k j)) (print "hata 2")))) 
    (cond ((not (eqn j (car (divide k i)))) (progn (prin1 i) (prin1 (divide k i)) (print "hata 3"))))
    (cond ((not (zerop (cdr (divide k i)))) (progn (prin1 i) (prin1 (divide k i)) (print "hata 4"))))
    (setq i (add1 i))
    (go l1)
l3  (setq l (add1 l))
    (cond ((eqn l 148) (return l))
          (t (go l2))) 
  )
) 

(test!-bolu)
