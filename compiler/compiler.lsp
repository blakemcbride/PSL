(global '(!*module !*msg !*nolinke !*ord !*plap !*r2i genfunction))
(setq !*msg t)

(de !&mkfunc (u) (mkquote u))
(de lstchr (u v)
   (cond
      ((null (cdr u)) (cons (car u) (cons nil v)))
      (t (cons (car u) (cons (list (lstchr (cdr u) v)) nil)))) )
(global '(erfg!* maxnargs))
(setq maxnargs 15)
(fluid
   '(alsts codelist condtail dfprint!* exit flagg freelst golist
       iregs iregs1 jmplist lblist llngth name narg regs regs1 slst
       stlst stomap switch))
(flag
   '(!*alloc !*dealloc !*entry !*exit !*store !*jump !*jumpc !*jumpnc
       !*jumpnil !*jumpt !*jumpe !*jumpn !*lbl !*lambind !*progbind
       !*freerstr !*load !*link !*linke !*code)
   'mc)
(de atsoc (u v)
   (cond
      ((null v) nil)
      ((eq u (caar v)) (car v))
      (t (atsoc u (cdr v)))) )
(de eqcar (u v) (and (not (atom u)) (eq (car u) v)))
(de lpri (u)
   (prog (oft)
      (setq oft (wrs nil))
      (cond
         ((atom u) (progn (prin2 u) (prin2 " ")))
         (t (mapc
               u
               (function
                  (lambda (x) (progn (prin2 x) (prin2 " ")))) )))
      (wrs oft)))
(de lprie (u)
   (progn
      (lpri (cons "*****" (cond ((atom u) (list u)) (t u))))
      (setq erfg!* t)
      (terpri)))
(de lprim (u)
   (cond
      (!*msg
         (progn
            (terpri)
            (lpri (cons "***" (cond ((atom u) (list u)) (t u))))
            (terpri)))) )
(de mkquote (u) (list 'quote u))
(de rplacw (a b) (rplaca (rplacd a (cdr b)) (car b)))
(de compile (x)
   (prog (exp)
      (mapc
         x
         (function
            (lambda (y)
               (cond
                  ((null (setq exp (getd y)))
                     (lprim (list y 'undefined)))
                  (t (compd y (car exp) (cdr exp)))) )))
      (return x)))
(de compd (name type exp)
   (prog nil
      (cond
         ((not (flagp type 'compile))
            (progn
               (lprim
                  (list "uncompilable function" name "of type" type))
               (return nil))))
      (cond
         ((not (atom exp))
            (cond
               (dfprint!*
                  (apply
                     dfprint!*
                     (list
                        (cond
                           ((eq type 'expr)
                              (cons 'de (cons name (cdr exp))))
                           ((eq type 'fexpr)
                              (cons 'df (cons name (cdr exp))))
                           ((eq type 'macro)
                              (cons 'dm (cons name (cdr exp))))
                           (t (list
                                 'putd
                                 (mkquote name)
                                 (mkquote type)
                                 (mkquote exp)))) )))
               (t (prog (x)
                     (cond
                        ((flagp type 'compile)
                           (put name 'cfntype (list type))))
                     (setq x
                        (cons
                           (list
                              '!*entry
                              name
                              type
                              (length (cadr exp)))
                           (!&comproc
                              exp
                              (cond ((flagp type 'compile) name)))) )
                     (cond (!*plap (mapc x (function print))))
                     (lap x)
                     (cond
                        ((and
                            (setq x (get name 'cfntype))
                            (eqcar (getd name) (car x)))
                           (remprop name 'cfntype)))) ))) )
      (return name)))
(flag '(expr fexpr macro) 'compile)
(de !&comproc (exp name)
   (prog (codelist flagg iregs iregs1 jmplist lblist llngth regs
         regs1 alsts exit slst stlst stomap condtail freelst switch
         narg)
      (setq narg 0)
      (setq llngth (list 1))
      (setq narg 0)
      (setq exit (!&genlbl))
      (setq stomap '((nil 1)))
      (setq codelist (list (cons '!*alloc llngth)))
      (setq exp (!&pass1 exp))
      (cond
         ((greaterp (length (cadr exp)) maxnargs)
            (lprie (list "too many args for compiler in" name))))
      (mapc
         (cadr exp)
         (function
            (lambda (z)
               (progn
                  (!&frame z)
                  (setq narg (add1 narg))
                  (cond
                     ((not (nonlocal z))
                        (setq iregs
                           (nconc iregs (list (list narg z)))) ))
                  (setq regs (nconc regs (list (list narg z)))) ))) )
      (cond ((null regs) (setq regs (list (cons 1 nil)))) )
      (setq alsts (!&freebind (cadr exp) t))
      (!&pass2 (caddr exp))
      (!&freerst alsts 0)
      (!&pass3)
      (rplaca llngth (difference 1 (car llngth)))
      (return codelist)))
(de nonlocal (x)
   (cond ((fluidp x) 'fluid) ((globalp x) 'global) (t nil)))
(de !&pass1 (exp) (!&pa1 exp nil))
(de !&pa1 (u vbls)
   (prog (x)
      (return
         (cond
            ((atom u)
               (cond
                  ((or (constantp u) (memq u '(nil t))) (mkquote u))
                  ((memq u vbls) u)
                  ((nonlocal u) u)
                  (t (progn (mknonlocal u) u))))
            ((not (atom (car u)))
               (cons (!&pa1 (car u) vbls) (!&palis (cdr u) vbls)))
            ((setq x (get (car u) 'pa1fn)) (apply x (list u vbls)))
            ((and
                (setq x (getd (car u)))
                (eq (car x) 'macro)
                (not (get (car u) 'compfn)))
               (!&pa1 (apply (cdr x) (list u)) vbls))
            ((setq x (get (car u) 'cmacro))
               (!&pa1
                  (sublis (pair (cadr x) (cdr u)) (caddr x))
                  vbls))
            ((and
                (eq (!&cfntype (car u)) 'fexpr)
                (not (get (car u) 'compfn)))
               (list (car u) (mkquote (cdr u))))
            ((or (memq (car u) vbls) (fluidp (car u)))
               (list 'apply (car u) (!&palist (cdr u) vbls)))
            (t (cons (car u) (!&palis (cdr u) vbls)))) )))
(de !&paiden (u vbls) u)
(put 'go 'pa1fn '!&paiden)
(put 'quote 'pa1fn '!&paiden)
(put 'code 'pa1fn '!&paiden)
(de !&pacond (u vbls)
   (cons
      'cond
      (mapcar
         (cdr u)
         (function
            (lambda (z)
               (list
                  (!&pa1 (car z) vbls)
                  (!&pa1 (!&mkprogn (cdr z)) vbls)))) )))
(put 'cond 'pa1fn '!&pacond)
(de !&pafunc (u vbls)
   (cond
      ((atom (cadr u)) (!&mkfunc (cadr u)))
      (t (!&mkfunc (storefunc (!&mknam name) 'expr (cadr u)))) ))
(put 'function 'pa1fn '!&pafunc)
(de !&palamb (u vbls)
   (cons
      'lambda
      (list
         (cadr u)
         (!&pa1 (!&mkprogn (cddr u)) (append (cadr u) vbls)))) )
(put 'lambda 'pa1fn '!&palamb)
(de !&palist (u vbls) (cons 'list (!&palis u vbls)))
(de !&paprog (u vbls)
   (cons
      'prog
      (cons (cadr u) (!&paprog1 (cddr u) (append (cadr u) vbls)))) )
(de !&paprog1 (u vbls)
   (mapcar
      u
      (function
         (lambda (x) (cond ((atom x) x) (t (!&pa1 x vbls)))) )))
(put 'prog 'pa1fn '!&paprog)
(de !&palis (u vbls)
   (mapcar u (function (lambda (x) (!&pa1 x vbls)))) )
(de mknonlocal (u)
   (progn
      (lprim (list u "declared fluid"))
      (fluid (list u))
      (list 'fluid u)))
(de !&mknam (u)
   (intern (compress (append (explode u) (explode (gensym)))) ))
(de !&mkprogn (u)
   (cond ((or (null u) (cdr u)) (cons 'progn u)) (t (car u))))
(deflist
   '((caar (lambda (u) (car (car u))))
       (cadr (lambda (u) (car (cdr u))))
       (cdar (lambda (u) (cdr (car u))))
       (cddr (lambda (u) (cdr (cdr u))))
       (caaar (lambda (u) (car (car (car u)))) )
       (caadr (lambda (u) (car (car (cdr u)))) )
       (cadar (lambda (u) (car (cdr (car u)))) )
       (caddr (lambda (u) (car (cdr (cdr u)))) )
       (cdaar (lambda (u) (cdr (car (car u)))) )
       (cdadr (lambda (u) (cdr (car (cdr u)))) )
       (cddar (lambda (u) (cdr (cdr (car u)))) )
       (cdddr (lambda (u) (cdr (cdr (cdr u)))) )
       (not (lambda (u) (null u))))
   'cmacro)
(de !&pass2 (exp) (!&comval exp 0))
(de !&comval (exp status)
   (cond
      ((!&anyreg exp nil)
         (cond ((greaterp status 1) nil) (t (!&lreg1 exp status))))
      (t (!&comval1 exp stomap status))))
(de !&comval1 (exp stomap status)
   (prog (x)
      (cond
         ((atom exp)
            (cond ((lessp status 2) (!&lreg1 exp status)) (t nil)))
         ((not (atom (car exp)))
            (cond
               ((eq (caar exp) 'lambda)
                  (!&comply (car exp) (cdr exp) status))
               (t (lprie (list "invalid function" (car exp)))) ))
         ((setq x (get (car exp) 'compfn))
            (apply x (list exp status)))
         ((and
             !*r2i
             (eq (car exp) name)
             (zerop status)
             (null freelst))
            (!&comrec exp status))
         ((eq (car exp) 'lambda)
            (lprie (list "invalid use of lambda in function" name)))
         ((eq (car exp) '!*code) (!&attach exp))
         (t (!&call (car exp) (cdr exp) status)))
      (return nil)))
(de !&anyreg (u v)
   (cond
      ((eqcar u 'quote) t)
      (t (and
            (or
               (atom u)
               (and
                  (idp (car u))
                  (get (car u) 'anyreg)
                  (!&anyreg (cadr u) nil)))
            (or (null !*ord) (!&anyregl v)))) ))
(de !&anyregl (u)
   (or (null u) (and (!&anyreg (car u) nil) (!&anyregl (cdr u)))) )
(de !&call (fn args status) (!&call1 fn (!&comlis args) status))
(de !&call1 (fn args status)
   (prog (argno)
      (informuser ".")
      (setq argno 0)
      (setq argno (length args))
      (!&loadargs args status)
      (!&attach (list '!*link fn (!&cfntype fn) argno))
      (cond
         ((flagp fn 'onereg)
            (setq regs (cons (cons 1 nil) (cdr regs))))
         ((flagp fn 'tworeg)
            (setq regs (cons (cons 1 nil) (delasc 2 (cdr regs)))) )
         (t (setq regs (list (cons 1 nil)))) )))
(de delasc (u v)
   (cond
      ((null v) nil)
      ((equal u (caar v)) (cdr v))
      (t (cons (car v) (delasc u (cdr v)))) ))
(de !&comlis (exp)
   (prog (acused y)
      (prog nil
   !$label
         (cond
            (exp
               (progn
                  (cond
                     ((!&anyreg (car exp) (cdr exp))
                        (setq y (cons (car exp) y)))
                     (t (progn
                           (cond (acused (!&store1)))
                           (!&comval1 (car exp) stomap 1)
                           (setq acused (gensym))
                           (setq regs
                              (cons
                                 (cons 1 (cons acused (cdar regs)))
                                 (cdr regs)))
                           (setq y (cons acused y)))) )
                  (setq exp (cdr exp))
                  (go !$label)))) )
      (return y)))
(de !&store1 nil
   (prog (x)
      (setq x (cadar regs))
      (cond
         ((or (null x) (eqcar x 'quote)) (return nil))
         ((not (atsoc x stomap)) (!&frame x)))
      (!&store0 x 1)))
(de !&comply (fn args status)
   (prog (alsts vars i)
      (setq i 0)
      (setq vars (cadr fn))
      (!&loadargs (!&comlis args) 1)
      (setq args (!&remvarl vars))
      (setq i 1)
      (mapc
         vars
         (function
            (lambda (v)
               (progn
                  (!&frame v)
                  (setq regs (!&repasc i v regs))
                  (setq i (add1 i)))) ))
      (setq alsts (!&freebind vars t))
      (setq i 1)
      (mapc
         vars
         (function
            (lambda (v)
               (progn
                  (cond ((not (nonlocal v)) (!&store0 v i)))
                  (setq i (add1 i)))) ))
      (!&comval (caddr fn) status)
      (!&freerst alsts status)
      (!&rstvarl vars args)))
(de !&comrec (exp status)
   (prog (x z)
      (!&loadargs (!&comlis (cdr exp)) status)
      (setq z codelist)
      (cond
         ((null (cdr z))
            (lprie (list "circular definition for" (car exp)))) )
      (prog nil
   !$label
         (cond ((cddr z) (progn (setq z (cdr z)) (go !$label)))) )
      (cond
         ((eq (caar z) '!*lbl) (setq x (cdar z)))
         (t (progn
               (setq x (!&genlbl))
               (rplacd z (list (cons '!*lbl x) (cadr z)))) ))
      (!&attjmp x)))
(de !&loadargs (args status)
   (prog (n)
      (setq n 0)
      (setq n (length args))
      (cond
         ((greaterp n maxnargs)
            (lprie (list "too many arguments in" name))))
      (cond ((greaterp status 0) (!&clrregs)))
      (prog nil
   !$label
         (cond
            (args
               (progn
                  (!&lreg n (car args) (cdr args) status)
                  (setq n (sub1 n))
                  (setq args (cdr args))
                  (go !$label)))) )))
(de !&locate (x)
   (prog (y vtype)
      (cond
         ((eqcar x 'quote) (return (list x)))
         ((setq y (!&rassoc x regs)) (return (list (car y))))
         ((not (atom x))
            (return (list (cons (car x) (!&locate (cadr x)))) ))
         ((setq vtype (nonlocal x)) (return (list (list vtype x)))) )
      (prog nil
   !$label
         (cond
            ((setq y (atsoc x slst))
               (progn (setq slst (delete y slst)) (go !$label)))) )
      (return
         (cond
            ((setq y (atsoc x stomap)) (cdr y))
            (t (list (mknonlocal x)))) )))
(de !&lreg (reg u v status)
   (prog (x y)
      (cond
         ((and (setq x (assoc reg regs)) (member u (cdr x)))
            (return nil))
         ((and
             (setq y (assoc reg iregs))
             (or (greaterp status 0) (!&memlis (cadr y) v)))
            (progn
               (!&store0 (cadr y) reg)
               (setq iregs (delete y iregs)))) )
      (!&attach (cons '!*load (cons reg (!&locate u))))
      (setq regs (!&repasc reg u regs))))
(de !&lreg1 (x status) (!&lreg 1 x nil status))
(de !&freebind (vars lambp)
   (prog (falst fregs x y i)
      (setq i 0)
      (setq i 1)
      (mapc
         vars
         (function
            (lambda (x)
               (progn
                  (cond
                     ((fluidp x)
                        (progn
                           (setq falst
                              (cons (cons x (!&getffrm x)) falst))
                           (setq fregs (cons i fregs))))
                     ((globalp x)
                        (lprie (list "cannot bind global " x))))
                  (setq i (add1 i)))) ))
      (cond ((null falst) (return nil)))
      (cond
         (lambp (!&attach (list '!*lambind fregs falst)))
         (t (!&attach (list '!*progbind falst))))
      (return falst)))
(de !&freerst (alsts status)
   (cond (alsts (!&attach (list '!*freerstr alsts)))) )
(de !&attach (u) (setq codelist (cons u codelist)))
(de !&store0 (u reg)
   (prog (x)
      (setq x (cons '!*store (cons reg (!&getfrm u))))
      (setq stlst (cons x stlst))
      (!&attach x)
      (cond
         ((atom u)
            (progn
               (!&clrstr u)
               (setq slst (cons (cons u codelist) slst)))) )))
(de !&clrstr (var)
   (prog (x)
      (cond (condtail (return nil)))
      (setq x (atsoc var slst))
      (cond ((null x) (return nil)))
      (setq stlst (!&deleq (cadr x) stlst))
      (setq slst (!&deleq x slst))
      (rplaca (cadr x) '!*noop)))
(de !&comtst (exp labl)
   (prog (x)
      (prog nil
   !$label
         (cond
            ((eqcar exp 'null)
               (progn
                  (setq switch (not switch))
                  (setq exp (cadr exp))
                  (go !$label)))) )
      (cond
         ((and
             (not (atom exp))
             (atom (car exp))
             (setq x (get (car exp) 'comtst)))
            (apply x (list exp labl)))
         (t (progn
               (cond
                  ((equal exp '(quote t))
                     (cond
                        (switch (!&attjmp labl))
                        (t (setq flagg t))))
                  (t (progn
                        (!&comval exp 1)
                        (!&attach
                           (list
                              (cond (switch '!*jumpt) (t '!*jumpnil))
                              (car labl)))
                        (!&addjmp codelist))))
               (setq regs1 regs)
               (setq iregs1 iregs))))
      (cond
         ((eqcar (car codelist) '!*jumpt)
            (setq regs
               (cons (cons 1 (cons '(quote nil) (cdar regs))) (cdr regs))))
         ((eqcar (car codelist) '!*jumpnil)
            (setq regs1
               (cons
                  (cons 1 (cons '(quote nil) (cdar regs1)))
                  (cdr regs1)))) )))
(de !&comandor (exp status)
   (prog (fn labl iregsl regsl)
      (setq fn (eq (car exp) 'and))
      (setq labl (!&genlbl))
      (cond
         ((greaterp status 1)
            (prog (regs1)
               (!&tstandor exp labl)
               (setq regs (!&rmerge2 regs regs1))))
         (t (prog nil
               (cond ((greaterp status 0) (!&clrregs)))
               (setq exp (cdr exp))
               (prog nil
            !$label
                  (cond
                     (exp
                        (progn
                           (!&comval
                              (car exp)
                              (cond ((cdr exp) 1) (t status)))
                           (setq iregsl (cons iregs iregsl))
                           (setq regsl (cons regs regsl))
                           (cond
                              ((cdr exp)
                                 (progn
                                    (!&attach
                                       (list
                                          (cond
                                             (fn '!*jumpnil)
                                             (t '!*jumpt))
                                          (car labl)))
                                    (!&addjmp codelist))))
                           (setq exp (cdr exp))
                           (go !$label)))) )
               (setq iregs (!&rmerge iregsl))
               (setq regs (!&rmerge regsl)))) )
      (!&attlbl labl)))
(de !&tstandor (exp labl)
   (prog (flg flg1 fn lab2 regsl regs1l tailp)
      (setq flg switch)
      (setq switch nil)
      (setq fn (eq (car exp) 'and))
      (setq flg1 (eq flg fn))
      (setq exp (cdr exp))
      (setq lab2 (!&genlbl))
      (!&clrregs)
      (prog nil
   !$label
         (cond
            (exp
               (progn
                  (setq switch nil)
                  (cond
                     ((and (null (cdr exp)) flg1)
                        (progn
                           (cond (fn (setq switch t)))
                           (!&comtst (car exp) labl)
                           (setq regsl (cons regs regsl))
                           (setq regs1l (cons regs1 regs1l))))
                     (t (progn
                           (cond ((not fn) (setq switch t)))
                           (cond
                              (flg1
                                 (progn
                                    (!&comtst (car exp) lab2)
                                    (setq regsl (cons regs1 regsl))
                                    (setq regs1l
                                       (cons regs regs1l))))
                              (t (progn
                                    (!&comtst (car exp) labl)
                                    (setq regsl (cons regs regsl))
                                    (setq regs1l
                                       (cons regs1 regs1l)))) ))) )
                  (cond
                     ((null tailp)
                        (progn
                           (setq condtail (cons nil condtail))
                           (setq tailp t))))
                  (setq exp (cdr exp))
                  (go !$label)))) )
      (!&attlbl lab2)
      (setq regs
         (cond ((not flg1) (car regsl)) (t (!&rmerge regsl))))
      (setq regs1 (cond (flg1 (car regs1l)) (t (!&rmerge regs1l))))
      (cond (tailp (setq condtail (cdr condtail))))
      (setq switch flg)))
(put 'and 'compfn '!&comandor)
(put 'or 'compfn '!&comandor)
(put 'and 'comtst '!&tstandor)
(put 'or 'comtst '!&tstandor)
(de !&comcond (exp status)
   (prog (iregs1 regs1 flagg switch lab1 lab2 regsl iregsl tailp)
      (setq exp (cdr exp))
      (setq lab1 (!&genlbl))
      (cond ((greaterp status 0) (!&clrregs)))
      (mapc
         exp
         (function
            (lambda (x)
               (progn
                  (setq lab2 (!&genlbl))
                  (setq switch nil)
                  (cond
                     ((cdr x) (!&comtst (car x) lab2))
                     (t (progn
                           (!&comval (car x) 1)
                           (!&attach (list '!*jumpnil (car lab2)))
                           (!&addjmp codelist)
                           (setq iregs1 iregs)
                           (setq regs1
                              (cons
                                 (cons 1 (cons '(quote nil) (cdar regs)))
                                 (cdr regs)))) ))
                  (cond
                     ((null tailp)
                        (progn
                           (setq condtail (cons nil condtail))
                           (setq tailp t))))
                  (!&comval (cadr x) status)
                  (cond
                     ((not (!&transferp (car codelist)))
                        (progn
                           (!&attjmp lab1)
                           (setq iregsl (cons iregs iregsl))
                           (setq regsl (cons regs regsl)))) )
                  (setq regs regs1)
                  (setq iregs iregs1)
                  (setq iregs1 nil)
                  (!&attlbl lab2)))) )
      (cond
         ((and (null flagg) (lessp status 2))
            (progn
               (!&lreg1 '(quote nil) status)
               (setq iregs (!&rmerge1 iregs iregsl))
               (setq regs (!&rmerge1 regs regsl))))
         (regsl
            (progn
               (setq iregs (!&rmerge1 iregs iregsl))
               (setq regs (!&rmerge1 regs regsl)))) )
      (!&attlbl lab1)
      (cond (tailp (setq condtail (cdr condtail)))) ))
(de !&rmerge (u)
   (cond ((null u) nil) (t (!&rmerge1 (car u) (cdr u)))) )
(de !&rmerge1 (u v)
   (cond
      ((null v) u)
      (t (!&rmerge1 (!&rmerge2 u (car v)) (cdr v)))) )
(de !&rmerge2 (u v)
   (cond
      ((or (null u) (null v)) nil)
      (t ((lambda (x)
             (cond
                (x
                   (cons
                      (cons (caar u) (xn (cdar u) (cdr x)))
                      (!&rmerge2 (cdr u) (delete x v))))
                (t (!&rmerge2 (cdr u) v))))
            (assoc (caar u) v)))) )
(flag '(!*jump !*linke error) 'transfer)
(put 'cond 'compfn '!&comcond)
(de !&comcons (exp status)
   (cond
      ((or (null (setq exp (cdr exp))) (null (cdr exp)) (cddr exp))
         (lprie "mismatch of arguments"))
      ((equal (cadr exp) '(quote nil))
         (!&call 'ncons (list (car exp)) status))
      ((and
          (eqcar (!&rassoc (cadr exp) regs) 1)
          (!&anyreg (car exp) nil))
         (!&call1 'xcons (!&comlis (reverse exp)) status))
      ((!&anyreg (cadr exp) nil) (!&call 'cons exp status))
      (t (!&call1 'xcons (reversip (!&comlis exp)) status))))
(put 'cons 'compfn '!&comcons)
(de !&comgo (exp status)
   (progn
      (!&clrregs)
      (cond
         ((greaterp status 2)
            (progn (!&attjmp (!&getlbl (cadr exp))) (setq slst nil)))
         (t (lprie (list exp "invalid")))) ))
(put 'go 'compfn '!&comgo)
(de !&comlist (exp status)
   (prog (m n fn)
      (setq exp (cdr exp))
      (setq m (min maxnargs 5))
      (setq n (length exp))
      (cond
         ((zerop n) (!&lreg1 '(quote nil) status))
         ((greaterp n m) (!&comval (!&comlist1 exp) status))
         (t (!&call
               (cond
                  ((onep n) 'ncons)
                  ((eqn n 2) 'list2)
                  ((eqn n 3) 'list3)
                  ((eqn n 4) 'list4)
                  (t 'list5))
               exp
               status)))) )
(de !&comlist1 (exp)
   (cond
      ((null exp) '(quote nil))
      (t (list 'cons (car exp) (cons 'list (cdr exp)))) ))
(put 'list 'compfn '!&comlist)
(de !&pamap (u vars)
   (cond
      ((eqcar (caddr u) 'function)
         ((lambda (x)
             (list
                (car u)
                (!&pa1 (cadr u) vars)
                (mkquote (cond ((atom x) x) (t (!&pa1 x vars)))) ))
            (cadr (caddr u))))
      (t (cons (car u) (!&palis (cdr u) vars)))) )
(put 'map 'pa1fn '!&pamap)
(put 'mapc 'pa1fn '!&pamap)
(put 'mapcan 'pa1fn '!&pamap)
(put 'mapcar 'pa1fn '!&pamap)
(put 'mapcon 'pa1fn '!&pamap)
(put 'maplist 'pa1fn '!&pamap)
(de !&commap (exp status)
   (prog (body fn lab1 lab2 lab3 tmp mtype result slst1 var x)
      (setq body (cadr exp))
      (setq fn (caddr exp))
      (setq lab1 (!&genlbl))
      (setq lab2 (!&genlbl))
      (setq mtype
         (cond
            ((memq (car exp) '(mapcar maplist)) 'cons)
            ((memq (car exp) '(mapcan mapcon))
               (progn (setq lab3 (!&genlbl)) 'nconc))
            (t nil)))
      (!&clrregs)
      (cond
         (mtype
            (progn
               (!&frame (setq result (gensym)))
               (cond ((null lab3) (!&store0 result nil)))) ))
      (!&frame (setq var (gensym)))
      (!&comval body 1)
      (setq regs (list (list 1 var)))
      (cond
         (lab3
            (progn
               (!&store0 var 1)
               (!&frame (setq tmp (gensym)))
               (!&comval '(ncons 'nil) 1)
               (!&store0 result 1)
               (!&store0 tmp 1)
               (!&lreg1 var 1))))
      (!&attjmp lab2)
      (!&attlbl lab1)
      (!&store0 var 1)
      (setq x
         (cond
            ((memq (car exp) '(map mapcon maplist)) var)
            (t (list 'car var))))
      (cond ((eqcar fn 'quote) (setq fn (cadr fn))))
      (setq slst1 slst)
      (!&comval (list fn x) (cond (mtype 1) (t 3)))
      (cond
         (mtype
            (progn
               (cond
                  (lab3
                     (progn
                        (!&attach (list '!*jumpnil (car lab3)))
                        (!&addjmp codelist)
                        (!&attach '(!*load 2 1))
                        (!&lreg1 tmp 1)
                        (!&store0 tmp 2)
                        (!&attach '(!*link nconc expr 2))
                        (!&attlbl lab3)))
                  (t (progn
                        (!&lreg 2 result nil 1)
                        (!&attach '(!*link cons expr 2))
                        (!&store0 result 1))))
               (setq regs (list (cons 1 nil)))) ))
      (setq slst (xn slst slst1))
      (!&comval (list 'cdr var) 1)
      (!&attlbl lab2)
      (!&attach (list '!*jumpt (car lab1)))
      (!&addjmp codelist)
      (cond
         (mtype
            (!&comval
               (list (cond (lab3 'cdr) (t 'reversip)) result)
               1))
         (t (setq regs (list (list 1 (mkquote nil)))) ))) )
(de xn (u v)
   (cond
      ((null u) nil)
      ((member (car u) v)
         (cons (car u) (xn (cdr u) (delete (car u) v))))
      (t (xn (cdr u) v))))
(put 'map 'compfn '!&commap)
(put 'mapc 'compfn '!&commap)
(put 'mapcan 'compfn '!&commap)
(put 'mapcar 'compfn '!&commap)
(put 'mapcon 'compfn '!&commap)
(put 'maplist 'compfn '!&commap)
(de !&comprog (exp status)
   (prog (alsts golist pg proglis exit i)
      (setq i 0)
      (setq proglis (cadr exp))
      (setq exp (cddr exp))
      (setq exit (!&genlbl))
      (setq pg (!&remvarl proglis))
      (mapc proglis (function !&frame))
      (setq alsts (!&freebind proglis nil))
      (mapc
         proglis
         (function
            (lambda (x)
               (cond ((not (nonlocal x)) (!&store0 x nil)))) ))
      (mapc
         exp
         (function
            (lambda (x)
               (cond
                  ((atom x)
                     (setq golist
                        (cons (cons x (!&genlbl)) golist)))) )))
      (prog nil
   !$label
         (cond
            (exp
               (progn
                  (cond
                     ((atom (car exp))
                        (progn
                           (!&clrregs)
                           (!&attlbl (!&getlbl (car exp)))
                           (setq regs (list (cons 1 nil)))) )
                     (t (!&comval
                           (car exp)
                           (cond ((greaterp status 2) 4) (t 3)))) )
                  (cond
                     ((and
                         (null (cdr exp))
                         (lessp status 2)
                         (or
                            (atom (car exp))
                            (not (memq (caar exp) '(go return)))) )
                        (setq exp (list '(return 'nil))))
                     (t (setq exp (cdr exp))))
                  (go !$label)))) )
      (!&attlbl exit)
      (cond
         ((cdr (!&findlbl exit)) (setq regs (list (cons 1 nil)))) )
      (!&freerst alsts status)
      (!&rstvarl proglis pg)))
(put 'prog 'compfn '!&comprog)
(de !&remvarl (vars) (mapcar vars (function !&remvar)))
(de !&remvar (x) (progn (!&remstores x) (!&protect x)))
(de !&remstores (x)
   (prog nil
      (mapc
         iregs
         (function
            (lambda (y)
               (cond
                  ((eq x (cadr y))
                     (progn
                        (!&store0 (cadr y) (car y))
                        (setq iregs (delete y iregs)))) ))) )
      (mapc
         regs
         (function
            (lambda (y)
               (prog nil
            !$label
                  (cond
                     ((member x (cdr y))
                        (progn
                           (rplacd y (!&deleq x (cdr y)))
                           (go !$label)))) ))) )))
(de !&protect (u)
   (prog (x)
      (cond ((setq x (atsoc u slst)) (setq slst (!&deleq x slst))))
      (return x)))
(de !&rstvarl (vars lst)
   (mapc
      vars
      (function
         (lambda (x)
            (progn
               (!&remstores x)
               (!&clrstr x)
               (!&unprotect (car lst))
               (setq lst (cdr lst)))) )))
(de !&unprotect (val) (cond (val (setq slst (cons val slst)))) )
(de !&comprogn (exp status)
   (prog nil
      (setq exp (cdr exp))
      (cond ((null exp) (return nil)))
      (prog nil
   !$label
         (cond
            ((cdr exp)
               (progn
                  (!&comval
                     (car exp)
                     (cond ((lessp status 2) 2) (t status)))
                  (setq exp (cdr exp))
                  (go !$label)))) )
      (!&comval (car exp) status)))
(put 'prog2 'compfn '!&comprogn)
(put 'progn 'compfn '!&comprogn)
(de !&comreturn (exp status)
   (progn
      (cond
         ((or (lessp status 4) (not (!&anyreg (cadr exp) nil)))
            (!&lreg1 (car (!&comlis (list (cadr exp)))) status)))
      (!&attjmp exit)))
(put 'return 'compfn '!&comreturn)
(de !&comsetq (exp status)
   (prog (x)
      (setq exp (cdr exp))
      (cond
         ((and
             (greaterp status 1)
             (or (null (cadr exp)) (equal (cadr exp) '(quote nil))))
            (!&store2 (car exp) nil))
         (t (progn
               (!&comval (cadr exp) 1)
               (!&store2 (car exp) 1)
               (cond
                  ((setq x (!&rassoc (car exp) iregs))
                     (setq iregs (delete x iregs))))
               (setq regs
                  (cons
                     (cons 1 (cons (car exp) (cdar regs)))
                     (cdr regs)))) ))) )
(de !&remsetvar (u v)
   (cond
      ((null v) nil)
      (t (cons
            (cons (caar v) (!&rems1 u (cdar v)))
            (!&remsetvar u (cdr v)))) ))
(de !&rems1 (u v)
   (cond
      ((null v) nil)
      ((smemq u (car v)) (!&rems1 u (cdr v)))
      (t (cons (car v) (!&rems1 u (cdr v)))) ))
(de smemq (u v)
   (cond
      ((atom v) (eq u v))
      ((eq (car v) 'quote) nil)
      (t (or (smemq u (car v)) (smemq u (cdr v)))) ))
(de !&store2 (u v)
   (prog (vtype)
      (setq regs (!&remsetvar u regs))
      (cond
         ((setq vtype (nonlocal u))
            (!&attach (list '!*store v (list vtype u))))
         ((not (atsoc u stomap))
            (!&attach (list '!*store v (mknonlocal u))))
         (t (!&store0 u v)))) )
(put 'setq 'compfn '!&comsetq)
(de !&comeq (exp labl)
   (prog (u v w)
      (setq u (cadr exp))
      (setq v (caddr exp))
      (cond
         ((member u (cdar regs)) (setq w (!&comeq1 v u)))
         ((member v (cdar regs)) (setq w (!&comeq1 u v)))
         ((!&anyreg v nil)
            (progn (!&comval u 1) (setq w (!&locate v))))
         ((!&anyreg u (list v))
            (progn (!&comval v 1) (setq w (!&locate u))))
         (t (progn
               (setq u (!&comlis (cdr exp)))
               (setq w (!&locate (cadr u)))) ))
      (!&attach
         (cons
            (cond (switch '!*jumpe) (t '!*jumpn))
            (cons (car labl) w)))
      (setq iregs1 iregs)
      (setq regs1 regs)
      (!&addjmp codelist)))
(de !&comeq1 (u v)
   (cond
      ((!&anyreg u (list v)) (!&locate u))
      (t (progn (!&comval u 1) (!&locate v)))) )
(put 'eq 'comtst '!&comeq)
(de !&testfn (exp labl)
   (prog (x)
      (cond
         ((not (setq x (!&rassoc (cadr exp) regs)))
            (!&comval (cadr exp) 1)))
      (!&clrregs)
      (!&attach
         (list
            (cond (switch '!*jumpc) (t '!*jumpnc))
            (car labl)
            (cond (x (car x)) (t 1))
            (car exp)))
      (setq regs1 regs)
      (!&addjmp codelist)))
(de !&memlis (u v)
   (and v (or (!&memb u (car v)) (!&memlis u (cdr v)))) )
(de !&memb (u v) (cond ((atom v) (eq u v)) (t (!&memb u (cadr v)))) )
(de !&rassoc (u v)
   (cond
      ((null v) nil)
      ((member u (cdar v)) (car v))
      (t (!&rassoc u (cdr v)))) )
(de !&repasc (reg u v)
   (cond
      ((null v) (list (list reg u)))
      ((equal reg (caar v)) (cons (list reg u) (cdr v)))
      (t (cons (car v) (!&repasc reg u (cdr v)))) ))
(de !&clrregs nil
   (prog nil
!$label
      (cond
         (iregs
            (progn
               (!&store0 (cadar iregs) (caar iregs))
               (setq iregs (cdr iregs))
               (go !$label)))) ))
(de !&cfntype (fn)
   (prog (x)
      (return
         (cond
            ((not (atom fn)) 'expr)
            ((setq x (get fn 'cfntype)) (car x))
            ((setq x (getd fn)) (car x))
            (t 'expr)))) )
(de !&genlbl nil
   (prog (l)
      (setq l (gensym))
      (setq lblist (cons (list l) lblist))
      (return (list l))))
(de !&getlbl (labl)
   (prog (x)
      (setq x (atsoc labl golist))
      (cond ((null x) (lprie (list labl " - missing label -"))))
      (return (cdr x))))
(de !&findlbl (lblst) (assoc (car lblst) lblist))
(de !&rechain (olbl nlbl)
   (prog (x y uses)
      (setq x (!&findlbl olbl))
      (setq y (!&findlbl nlbl))
      (rplaca olbl (car nlbl))
      (setq uses (cdr x))
      (rplacd x nil)
      (rplacd y (append uses (cdr y)))
      (mapc
         uses
         (function (lambda (x) (rplaca (cdr x) (car nlbl)))) )))
(de !&moveup (u)
   (cond
      ((eq (caadr u) '!*jump)
         (progn
            (setq jmplist (!&deleq (cdr u) jmplist))
            (rplacw u (cdr u))
            (setq jmplist (cons u jmplist))))
      (t (rplacw u (cdr u)))) )
(de !&attlbl (lbl)
   (cond
      ((eq (caar codelist) '!*lbl) (!&rechain lbl (cdar codelist)))
      (t (!&attach (cons '!*lbl lbl)))) )
(de !&attjmp (lbl)
   (prog nil
      (cond
         ((eq (caar codelist) '!*lbl)
            (progn
               (!&rechain (cdar codelist) lbl)
               (setq codelist (cdr codelist)))) )
      (cond ((!&transferp (car codelist)) (return nil)))
      (!&attach (cons '!*jump lbl))
      (!&addjmp codelist)))
(de !&transferp (x)
   (flagp
      (cond ((eq (car x) '!*link) (cadr x)) (t (car x)))
      'transfer))
(de !&addjmp (clist)
   (prog (x)
      (setq x (!&findlbl (cdar clist)))
      (rplacd x (cons (car clist) (cdr x)))
      (setq jmplist (cons clist jmplist))))
(de !&remjmp (clist)
   (prog (x)
      (setq x (!&findlbl (cdar clist)))
      (rplacd x (!&deleq (car clist) (cdr x)))
      (setq jmplist (!&deleq clist jmplist))
      (!&moveup clist)))
(de !&deleq (u v)
   (cond
      ((null v) nil)
      ((eq u (car v)) (cdr v))
      (t (cons (car v) (!&deleq u (cdr v)))) ))
(de !&frame (u)
   (prog (z)
      (setq stomap
         (cons
            (list u (setq z (sub1 (cadar stomap))))
            stomap))
      (cond ((lessp z (car llngth)) (rplaca llngth z)))) )
(de !&getfrm (u)
   ((lambda (x)
       (cond
          (x (cdr x))
          (t (lprie (list "compiler error: lost var" u)))) )
      (atsoc u stomap)))
(de !&getffrm (u)
   (prog (x)
      (setq x (!&getfrm u))
      (setq freelst (cons x freelst))
      (return x)))
(de !&pass3 nil
   (prog (flagg)
      (informuser "@")
      (mapc
         slst
         (function
            (lambda (j)
               (progn
                  (setq stlst (!&deleq (cadr j) stlst))
                  (rplaca (cadr j) '!*noop)))) )
      (!&fixchains)
      (!&fixlinks)
      (!&fixfrm)
      (!&attlbl exit)
      (cond
         (flagg
            (progn
               (cond
                  ((and
                      (not !*nolinke)
                      (eq (caar codelist) '!*lbl)
                      (eq (caadr codelist) '!*linke))
                     (rplaca
                        (cdr codelist)
                        (list
                           '!*link
                           (cadadr codelist)
                           (cadr (cdadr codelist))
                           (caddr (cdadr codelist)))) ))
               (!&attach (cons '!*dealloc llngth))
               (!&attach (list '!*exit)))) )
      (!&peepholeopt)
      (!&fixrest)))
(de !&fixchains nil
   (prog (ejmps ejmps1 p q)
      (cond
         ((not (equal (car codelist) (cons '!*lbl exit)))
            (!&attlbl exit)))
      (setq codelist (cdr codelist))
      (cond
         ((not (equal (car codelist) (cons '!*jump exit)))
            (!&attjmp exit)))
      (setq ejmps (reverse jmplist))
      (prog nil
   !$label
         (cond
            (ejmps
               (progn
                  (prog nil
                     (setq p (car ejmps))
                     (setq ejmps (cdr ejmps))
                     (cond
                        ((eq (caar p) '!*jump)
                           (progn
                              (setq ejmps1 ejmps)
                              (prog nil
                           !$label
                                 (cond
                                    (ejmps1
                                       (progn
                                          (cond
                                             ((and
      (equal (car p) (caar ejmps1))
      (equal (cadr p) (cadar ejmps1)))
                                                (progn
      (!&remjmp p)
      (!&fixchn p (cdar ejmps1))
      (setq ejmps1 nil)))
                                             (t (setq ejmps1
      (cdr ejmps1))))
                                          (go !$label)))) ))) ))
                  (go !$label)))) )))
(de !&fixlinks nil
   (prog (ejmps p q)
      (setq ejmps jmplist)
      (cond
         ((not !*nolinke)
            (prog nil
         !$label
               (cond
                  (ejmps
                     (progn
                        (prog nil
                           (setq p (car ejmps))
                           (setq q (cdr p))
                           (setq ejmps (cdr ejmps))
                           (cond
                              ((not (eq (cadar p) (car exit)))
                                 (return nil))
                              ((or
                                  (not (eq (caar p) '!*jump))
                                  (not (eq (caar q) '!*link)))
                                 (return (setq flagg t))))
                           (rplacw
                              (car q)
                              (cons
                                 '!*linke
                                 (cons
                                    (cadar q)
                                    (cons
                                       (caddar q)
                                       (cons
                                          (cadr (cddar q))
                                          llngth)))) )
                           (!&remjmp p))
                        (go !$label)))) ))
         (t (setq flagg t)))) )
(de !&findblk (u lbl)
   (cond
      ((null (cdr u)) nil)
      ((and (eq (caadr u) '!*lbl) (!&transferp (caddr u))) u)
      ((and (get (caadr u) 'negjmp) (eq (cadadr u) lbl)) u)
      (t (!&findblk (cdr u) lbl))))
(put '!*noop 'optfn '!&moveup)
(put '!*lbl 'optfn '!&lblopt)
(de !&lblopt (u)
   (prog (z)
      (cond
         ((eq (cadar u) (cadadr u)) (return (!&remjmp (cdr u))))
         ((and
             (eq (caadr u) '!*jump)
             (setq z (get (caaddr u) 'negjmp))
             (eq (cadar u) (cadr (caddr u))))
            (return
               (progn
                  (setq z
                     (cons z (cons (cadadr u) (cddr (caddr u)))) )
                  (!&remjmp (cdr u))
                  (!&remjmp (cdr u))
                  (rplacd u (cons z (cons (cadr u) (cddr u))))
                  (!&addjmp (cdr u))
                  t)))
         (t (return nil)))) )
(de !&peepholeopt nil
   (prog (x z)
      (setq z codelist)
      (prog nil
   !$label
         (cond
            (z
               (progn
                  (cond
                     ((or
                         (not (setq x (get (caar z) 'optfn)))
                         (not (apply x (list z))))
                        (setq z (cdr z))))
                  (go !$label)))) )))
(de !&fixrest nil
   (prog (labs tlabs x y z)
      (prog nil
   !$label
         (cond
            (codelist
               (progn
                  (cond
                     ((eq (caar codelist) '!*lbl)
                        (progn
                           (!&lblopt codelist)
                           (cond
                              ((cdr
                                  (setq z
                                     (!&findlbl (cdar codelist))))
                                 (progn
                                    (setq y (cons (car codelist) y))
                                    (cond
                                       ((and
                                           (null (cddr z))
                                           (!&transferp (cadr z))
                                           (eq (caadr y) '!*load)
                                           (!&noloadp
                                              (cdadr y)
                                              (cdr
      (atsoc (cadr z) jmplist))))
                                          (progn
                                             (cond
                                                ((not
      (!&noloadp (cdadr y) (cdr codelist)))
      (rplacw
      (cdr codelist)
      (cons (cadr y) (cons (cadr codelist) (cddr codelist)))) ))
                                             (rplacw
                                                (cdr y)
                                                (cddr y))))
                                       (t (progn
                                             (cond
                                                ((and
      (null (cddr z))
      (eq (caadr codelist) '!*jump) (get (caadr z) 'negjmp))
      (setq labs (cons (cons (cadr z) y) labs))))
                                             (cond
                                                ((!&transferp
      (cadr codelist))
   (setq tlabs (cons (cons (cadar y) y) tlabs)))) ))) ))) ))
                     ((and
                         (get (caar codelist) 'negjmp)
                (setq z (atsoc (car codelist) labs)))
                        (progn
                        (setq x (car codelist))
                        (setq codelist (cdr codelist))
                           (setq z (cddr z))
                           (prog nil
                        !$label
                              (cond
                                 ((and
                                     (equal (car y) (car z))
                                     (or
                                     (eq (caar y) '!*store)
                                     (and
                                  (eq (caar y) '!*load)
                                           (not (onep (cadar y))))) )
                                    (progn
                                       (setq codelist
                                          (cons (car y) codelist))
                                       (rplacw
                                          z
                                          (cons (cadr z) (cddr z)))
                                       (setq y (cdr y))
                                       (go !$label)))) )
                     (setq codelist (cons x codelist))
                           (setq y (cons x y))))
                     ((and
                         (eq (caar codelist) '!*jump)
                         (setq z (atsoc (cadar codelist) tlabs))
                         (setq x
                            (!&findblk
                               (cdr codelist)
                (cond ((eq (caar y) '!*lbl) (cadar y)) (t nil)))) )
                        (prog (w)
                           (cond
                           ((not (eq (caadr x) '!*lbl))
                                 (progn
                           (cond
                           ((not (eq (caar x) '!*lbl))
                                          (setq x
                                             (cdr
                                                (rplacd
                                                   x
      (cons (cons '!*lbl (!&genlbl)) (cdr x)))) )))
                                    (setq w
                                 (cons
                                 (get (caadr x) 'negjmp)
                                          (cons
                                    (cadar x)
                                       (cddadr x))))
                                    (!&remjmp (cdr x))
                                    (rplacd
                                       x
                                       (cons
                                          w
                                          (cons (cadr x) (cddr x))))
                                 (!&addjmp (cdr x))))
                              (t (setq x (cdr x))))
                           (setq w nil)
                           (setq w (cons (car y) w))
                           (setq y (cdr y))
                           (prog nil
                        !$label
                              (cond
                                 ((not (eq y (cdr z)))
                           (progn
                              (setq w (cons (car y) w))
                                       (setq y (cdr y))
                                       (go !$label)))) )
                           (rplacd x (nconc w (cdr x)))
                           (!&remjmp codelist)
                           (setq tlabs nil)
                           (setq codelist
                              (cons nil (cons (car y) codelist)))
                           (setq y (cdr y))))
                     (t (setq y (cons (car codelist) y))))
                  (setq codelist (cdr codelist))
                  (go !$label)))) )
      (setq codelist y)))
(de !&noloadp (args instrs)
   (and
      (atom (cadr args))
      (or
         (and (eq (caar instrs) '!*load) (equal (cdar instrs) args))
         (and
            (eq (caar instrs) '!*store)
            (or
               (equal (cdar instrs) args)
               (and
                  (not (equal (caddar instrs) (cadr args)))
                  (!&noloadp args (cdr instrs)))) ))) )
(de !&fixchn (u v)
   (prog (x)
      (prog nil
   !$label
         (cond
            ((equal (car u) (car v))
               (progn (!&moveup u) (setq v (cdr v)) (go !$label)))) )
      (setq x (!&genlbl))
      (cond
         ((eq (caar v) '!*lbl) (!&rechain x (cdar v)))
         (t (rplacw
               v
               (cons (cons '!*lbl x) (cons (car v) (cdr v)))) ))
      (cond
         ((eq (caar u) '!*lbl)
            (progn (!&rechain (cdar u) x) (!&moveup u))))
      (cond ((eq (caar u) '!*jump) (return nil)))
      (rplacw u (cons (cons '!*jump x) (cons (car u) (cdr u))))
      (!&addjmp u)))
(de !&fixfrm nil
   (prog (holes lst x y z n)
      (setq n 0)
      (cond
         ((and (null stlst) (null freelst))
            (return (rplaca llngth 1))))
      (setq n 0)
      (prog nil
   !$label
         (cond
            ((not (lessp n (car llngth)))
               (progn
                  (setq y nil)
                  (mapc
                     stlst
                     (function
                        (lambda (lst)
                           (cond
                              ((equal n (caddr lst))
                                 (setq y (cons (cddr lst) y)))) )))
                  (mapc
                     freelst
                     (function
                        (lambda (lst)
                           (cond
                              ((equal n (car lst))
                                 (setq y (cons lst y)))) )))
                  (cond
                     ((null y) (setq holes (cons n holes)))
                     (t (setq z (cons (cons n y) z))))
                  (setq n (sub1 n))
                  (go !$label)))) )
      (setq y z)
      (cond
         ((greaterp (caar z) (car llngth)) (rplaca llngth (caar z))))
      (prog nil
   !$label
         (cond
            (holes
               (progn
                  (prog nil
               !$label
                     (cond
                        ((and holes (lessp (car holes) (car llngth)))
                           (progn
                              (setq holes (cdr holes))
                              (go !$label)))) )
                  (cond
                     (holes
                        (progn
                           (setq holes (reversip holes))
                           (mapc
                              (cdar z)
                              (function
                                 (lambda (x)
                                    (rplaca x (car holes)))) )
                           (rplaca
                              llngth
                              (cond
                                 ((or
                                     (null (cdr z))
                                     (lessp (car holes) (caadr z)))
                                    (car holes))
                                 (t (caadr z))))
                           (setq holes (reversip (cdr holes)))
                           (setq z (cdr z)))) )
                  (go !$label)))) )
      (setq n (cond ((lessp narg 3) 3) (t (add1 narg))))
      (cond
         ((or
             freelst
             (null (!&regp codelist))
             (lessp (car llngth) (difference n maxnargs)))
            (return nil)))
      (mapc
         stlst
         (function
            (lambda (x)
               (rplacw
                  x
                  (list
                     '!*load
                     (difference n (caddr x))
                     (cond
                        ((null (cadr x)) '(quote nil))
                        (t (cadr x)))) ))) )
      (prog nil
   !$label
         (cond
            (y
               (progn
                  (mapc
                     (cdar y)
                     (function
                        (lambda (x)
                           (and
                              (not (greaterp (car x) 0))
                              (rplaca x (difference n (car x)))) )))
                  (setq y (cdr y))
                  (go !$label)))) )
      (rplaca llngth 1)))
(de !&regp (u)
   (cond
      ((null (cdr u)) t)
      ((and
          (memq (caar u) '(!*load !*store))
          (numberp (cadar u))
          (greaterp (cadar u) 2))
         nil)
      ((and
          (flagp (caadr u) 'unknownuse)
          (not
             (or
                (and
                   (idp (cadadr u))
                   (or
                      (flagp (cadadr u) 'onereg)
                      (flagp (cadadr u) 'tworeg)))
                (equal (car u) (cons '!*jump exit)))) )
         nil)
      (t (!&regp (cdr u)))) )
(flag '(!*code !*link !*linke) 'unknownuse)
(de !*code (u) (eval u))
(put '!*jumpn 'negjmp '!*jumpe)
(put '!*jumpe 'negjmp '!*jumpn)
(put '!*jumpnil 'negjmp '!*jumpt)
(put '!*jumpt 'negjmp '!*jumpnil)
(put '!*jumpc 'negjmp '!*jumpnc)
(put '!*jumpnc 'negjmp '!*jumpc)
(de !&paplus2 (u vars)
   (cond
      ((onep (caddr u)) (list 'add1 (!&pa1 (cadr u) vars)))
      ((onep (cadr u)) (list 'add1 (!&pa1 (caddr u) vars)))
      (t (cons 'plus2 (!&palis (cdr u) vars)))) )
(put 'plus2 'pa1fn '!&paplus2)
(de !&padiff (u vars)
   (cond
      ((onep (caddr u)) (list 'sub1 (!&pa1 (cadr u) vars)))
      (t (cons 'difference (!&palis (cdr u) vars)))) )
(put 'difference 'pa1fn '!&padiff)
(de !&palessp (u vars)
   (cond
      ((zerop (caddr u)) (list 'minusp (!&pa1 (cadr u) vars)))
      (t (cons 'lessp (!&palis (cdr u) vars)))) )
(put 'lessp 'pa1fn '!&palessp)
(de !&paminus (u vars)
   (cond
      ((and
          (eqcar (setq u (!&pa1 (cadr u) vars)) 'quote)
          (numberp (cadr u)))
         (mkquote (minus (cadr u))))
      (t (list 'minus u))))
(put 'minus 'pa1fn '!&paminus)
