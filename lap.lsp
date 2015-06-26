(global
   '(registers alphabet framelocs labels grandlab labinuse urwelt
     urlength !*cegal atlas calledf calledfloc compf !*maxfnamelen
     genfncount!* genfname!*))

(setq registers (mkvect 15))

(setq alphabet
   (prog (!$dummy!$ i)
      (setq i 65)
!$label
      (cond ((greaterp i 90) (return (reverse !$dummy!$))))
      (setq !$dummy!$
         (cons (cons (ascii (plus2 i 32)) (ascii i)) !$dummy!$))
      (setq !$dummy!$
         (cons (cons (ascii i) (ascii i)) !$dummy!$))
      (setq i (add1 i))
      (go !$label)))
(prog (i)
   (setq i 1)
!$label
   (cond ((greaterp i 15) (return nil)))
   (putv registers i (compress (append '(r e g) (explode i))))
   (setq i (add1 i))
   (go !$label))

(setq framelocs (mkvect 20))

(de storefunc (u v w)
   (progn
      (setq genfunction
         (cons (cons 'de (cons u (cdr w))) genfunction))
      u))
(df output1 (!&x)
   (mapc !&x
      (function
         (lambda (xwx)
            (cond
               ((null xwx) (progn (terpri) (prin2 "  ")))
               ((eq xwx t) (terpri))
               (t (prin2 (eval xwx)))) ))) )
(de span (n)
   (prog (j i)
      (setq n (sub1 n))
      (setq i 0)
!$label
      (cond ((greaterp i n) (return (reversip j))))
      (setq j (cons (minus i) j))
      (setq i (add1 i))
      (go !$label)))

(de setdiff (y x)
   (mapcan
      y
      (function
         (lambda (e) (cond ((member e x) nil) (t (list e)))) )))

(de del (x l)
   (cond (l 
            (cond ((equal x (car l))  (del x (cdr l)))
                  (t                  (cons (car l) (del x (cdr l))))))
         (t  nil)))

(de makelocal (i)
   (compress
      (append '(l o c a l !! !() (nconc (explode i) '(!! !))))) )
(prog (i)
   (setq i 0)
!$label
   (cond ((greaterp i 20) (return nil)))
   (putv framelocs i (makelocal i))
   (setq i (add1 i))
   (go !$label))
(setq labels
   (prog (!$dummy!$ i)
      (setq i 1)
!$label
      (cond ((greaterp i 90) (return (reverse !$dummy!$))))
      (setq !$dummy!$
         (cons (compress (append '(l a b) (explode i))) !$dummy!$))
      (setq i (add1 i))
      (go !$label)))
(de locs (i)
   (progn
      (cond (atlas (setq i (cdr (assoc i atlas)))) )
      (cond
         ((greaterp (abs i) 20) (makelocal (abs i)))
         (t (getv framelocs (abs i)))) ))
(setq urwelt nil)
(setq urlength -1)
(de inurwelt (x)
   (prog (s)
      (setq s (member x urwelt))
      (cond (s (return (sub1 (length s)))) )
      (setq urwelt (cons x urwelt))
      (return (setq urlength (add1 urlength)))) )

(de pexp (u)
   (progn
      (cond ((and (pairp u) (null (cdr u))) (setq u (car u))))
      (cond
         ((or (null u) (equal u '(quote nil))) (prin2 "NIL"))
         ((or (eq u t) (equal u '(quote t))) (prin2 "T"))
         ((numberp u)
            (prin2
               (cond
                  ((greaterp u 0) (getv registers u))
                  (t (locs u)))) )
         ((memq (car u) '(fluid global))
            (progn
               (prin2 "value(urwelt[")
               (prin2 (inurwelt (cadr u)))
               (prin2 "])")))
         ((eq (car u) 'quote)
          (cond ((or (null (fixp (setq u (cadr u))))
                 (greaterp u 7) (lessp u -1))
                 (progn
                     (prin2 "urwelt[")
                     (prin2 (inurwelt u))
                     (prin2 "]")) )
                (t
                 (cond ((zerop u) (prin2 "Sexp(&ZERO)"))
                       ((onep u)  (prin2 "Sexp(&ONE)"))
                       ((eqn u 2)  (prin2 "Sexp(&TWO)"))
                       ((eqn u 3)  (prin2 "Sexp(&THREE)"))
                       ((eqn u 4)  (prin2 "Sexp(&FOUR)"))
                       ((eqn u -1)  (prin2 "Sexp(&M_ONE)"))
                       ((eqn u 5)  (prin2 "Sexp(&FIVE)"))
                       ((eqn u 6)  (prin2 "Sexp(&SIX)"))
                       ((eqn u 7)  (prin2 "Sexp(&SEVEN)")))) ))
         (t (progn
               (prin2 (car u))
               (prin2 "(")
               (pexp (cadr u))
               (prin2 ")")))) ))

(setq !*maxfnamelen nil)

(de ersinize (u)
   (cond 
         ((and !*maxfnamelen
               (greaterp (length (explode u)) !*maxfnamelen))
          (genfname u))
         (t   (progn
                (setq u
                   (cond
                     ((flagp u 'specialname) (nconc (explode u) '(1)))
                     (t (killunlem (explode u)))) )
                (cond ((memq (car u) '(!1 !2 !3 !4 !5 !6 !7 !8 !9 !0))
                       (compress  (append '(!! !_) u)))
                      (t
                       (compress (cons (cdr (assoc (car u) alphabet)) (cdr u))))))
               ))) 

(setq genfname!* nil)
(setq genfncount!* 0)

(de genfname (u)
   (prog (v)
      (setq v (assoc u genfname!*))
      (cond (v (return (cdr v))))
      (setq genfncount!* (add1 genfncount!*))
      (setq v (compress (append '(G e n F u n) (explode genfncount!*))))
      (setq genfname!*  (cons (cons u v) genfname!*))
      (return v)
   ))

(de killunlem (u)
   (cond
      ((null u) nil)
      ((eq (car u) '!!)
         (progn
            (setq u (cdr u))
            (append
               (cdr (assoc (car u) !*cegal))
               (killunlem (cdr u)))) )
      (t (cons (car u) (killunlem (cdr u)))) ))

(setq !*cegal
   '((!  S P) (!! X L) (!" D Q) (!# N B) (!$ D L) (!% P S) (!& A N)
     (!' S Q) (!( L P) (!) R P) (!* A S) (!+ P L) (!, C M) (!- M N)
     (!. D T) (!/ S L) (!: C L) (!; S C) (!< L T) (!= E Q) (!> G T)
     (!? Q S) (!@ A T) (![ L B) (!\ B S) (!] R B) (!^ U P) (!` B Q)
     (!_ U S) (!{ L C) (!| O R) (!} R C) (!~ T L)
     (!1 !1) (!2 !2) (!3 !3) (!4 !4) (!5 !5) (!6 !6) (!7 !7) (!8 !8)   
     (!9 !9) (!0 !0)
))

(flag '(open close write read system) 'specialname)

(de last (u)
   (prog (x)
      (setq x u)
l     (cond ((null (cdr x)) (return (car x))))
      (setq x (cdr x))
      (go l)
nil  ))

(setq grandlab labels)
(setq labinuse nil)

(de fetchlab (u)
   (mapc u
      (function
         (lambda (x)
            (cond
               ((eq (car x) '!*lbl)
                  (rplacd x (list (newlab (cadr x)))) ))) )))

(de delrep (u)
   (cond ((null u) nil)
         ((member (car u) (cdr u)) (delrep (cdr u)))
         (t (cons (car u) (delrep (cdr u)))))  )

(de compactify (u)
   (prog (r s n)
      (cond ((or (null (setq n (assoc '!*alloc u))) (zerop (cadr n)) )
             (progn (setq atlas nil) (return nil)) ))
      (mapc u
         (function
            (lambda (e)
               (cond ((eq (car e) '!*freerstr)
                      (setq s (nconc s
                               (mapcar (cadr e) (function cadr)))) ))) ))
      (cond ((null s) (progn (setq atlas nil) (return nil)) ))
      (setq r (length s))
      (cond ((eqn r 2)
             (cond ((eqn (car s) (cadr s)) (setq s (cdr s)))) )
            ((greaterp r 2) (setq s (delrep s))) )
      (mapc u
         (function
            (lambda (e)
               (cond ((and (eq (car e) '!*store) (member (caddr e) s))
                      (setq s (delete (caddr e) s)))))))
      (cond ((null s) (progn (setq atlas nil) (return nil)) ))
      (setq n (cadr n))
      (setq r (difference n (length s)))
      (setq atlas
         (pair
            (setdiff (span n) s)
            (span r) ))
      (setq r (ncons r))
      (rplacd (assoc '!*alloc u) r)
      (rplacd (assoc '!*dealloc u) r)
   ))

(de newlab (u)
   (progn
      (setq labinuse (cons (cons u (car labels)) labinuse))
      (setq labels (cdr labels))
      (cdar labinuse)))
(de initlab nil (progn (setq labels grandlab) (setq labinuse nil)))
(de getlab (u) (cdr (assoc u labinuse)))
(de lap (zaz)
   (progn
      (setq labels grandlab)
      (fetchlab zaz)
      (compactify zaz)
      (mapc zaz (function lap1))))
(de lap1 (uuu)
   (eval
      (cons (car uuu)
         (mapcar (cdr uuu)
            (function (lambda (xyx)
                  (cond ((or (pairp xyx) (idp xyx)) (list 'quote xyx))
                         (t xyx))))
   ))) )
(de !*entry (name type narg)
   (progn
      (terpri)
      (prin2 "void ")
      (prin2 (ersinize name))
      (output1 "()     /* ")
      (prin1 name)
      (output1 " */" t "{")))

(de !*alloc (n)
   (cond
      ((not (zerop n))
         (progn (output1 nil "kalloc(") (prin2 n) (prin2 ");")))) )
(de !*dealloc (n)
   (cond
      ((not (zerop n))
         (progn (output1 nil "kpop(") (prin2 n) (prin2 ");")))) )
(de !*jump (labl)
   (progn (output1 nil "goto ") (prin2 (getlab labl)) (prin2 ";")))
(de !*jumpnil (labl)
   (progn
      (output1 nil "if (null(reg1)) goto ")
      (prin2 (getlab labl))
      (prin2 ";")))
(de !*jumpt (labl)
   (progn
      (output1 nil "if (!null(reg1)) goto ")
      (prin2 (getlab labl))
      (prin2 ";")))
(de !*jumpe (labl exp)
   (progn
      (output1 nil "if (reg1 == ")
      (pexp exp)
      (prin2 ") goto ")
      (prin2 (getlab labl))
      (prin2 ";")))
(de !*jumpn (labl exp)
   (progn
      (output1 nil "if (reg1 != ")
      (pexp exp)
      (prin2 ") goto ")
      (prin2 (getlab labl))
      (prin2 ";")))
(de !*jumpc (labl reg name)
   (progn
      (output1 nil "if (")
      (prin2 name)
      (prin2 "(")
      (prin2 (getv registers reg))
      (prin2 ")) goto ")
      (prin2 (getlab labl))
      (prin2 ";")))
(de !*jumpnc (labl reg name)
   (progn
      (output1 nil "if (!")
      (prin2 name)
      (prin2 "(")
      (prin2 (getv registers reg))
      (prin2 ")) goto ")
      (prin2 (getlab labl))
      (prin2 ";")))
(de !*lbl (labl) (progn (terpri) (prin2 labl) (prin2 " : ")))
(de !*store (nreg floc)
   (progn
      (output1 nil)
      (cond
         ((fixp floc)
            (progn
               (cond (atlas (setq floc (cdr (assoc floc atlas)))) )
               (cond
                  ((zerop floc) (prin2 "kset0("))
                  (t (progn
                        (prin2 "kset(")
                        (prin2 (abs floc))
                        (prin2 ","))))
               (pexp nreg)
               (prin2 ");")))
         (t (progn
               (pexp floc)
               (prin2 " = ")
               (pexp nreg)
               (prin2 ";")))) ))
(de !*load (reg exp)
   (cond
      ((or (equal reg exp) (and (pairp exp) (equal reg (car exp))))
         nil)
      (t (progn
            (output1 nil)
            (cond
               ((and (pairp exp) (null (cdr exp)))
                  (setq exp (car exp))))
            (cond
               ((and (numberp exp) (null (greaterp exp 0)))
                  (progn
                     (cond
                        (atlas (setq exp (cdr (assoc exp atlas)))) )
                     (cond
                        ((zerop exp)
                           (progn
                              (prin2 "kload0(")
                              (pexp reg)
                              (prin2 ");")))
                        (t (progn
                              (prin2 "kload(")
                              (pexp reg)
                              (prin2 ",")
                              (prin2 (abs exp))
                              (prin2 ");")))) ))
               (t (progn
                     (pexp reg)
                     (prin2 " = ")
                     (pexp exp)
                     (prin2 ";")))) ))) )
(de !*link (name type nargs)
   (progn
      (cond
         ((null (memq name calledfloc))
            (setq calledfloc (cons name calledfloc))))
      (output1 nil)
      (prin2 (ersinize name))
      (prin2 "();")
      (output1 "    /* ")
      (prin1  name)
      (output1 " */")
      (cond
         ((and (null (memq name compf)) (null (assoc name calledf)))
            (setq calledf (cons (list name type nargs) calledf)))) ))
(de !*linke (name type nargs n)
   (progn
      (!*dealloc n)
      (!*link name type nargs)
      (output1 nil "return;")))
(de !*lambind (regs alst)
   (mapc
      regs
      (function
         (lambda (reg)
            (progn
               (output1 nil "zbind(urwelt[")
               (prin2 (inurwelt (caar alst)))
               (prin2 "],")
               (prin2 (getv registers reg))
               (prin2 ");")
               (setq alst (cdr alst)))) )))
(de !*progbind (alst)
   (mapc
      alst
      (function
         (lambda (entry)
            (progn
               (output1 nil "zbind(urwelt[")
               (prin2 (inurwelt (caar alst)))
               (prin2 "],NIL);")
               (setq alst (cdr alst)))) )))
(de !*freerstr (alst)
   (progn
      (output1 nil "zunbind(")
      (prin2 (length alst))
      (prin2 ");")))
(de !&expandus (fn1 fn2 exp status)
   (progn
      (setq exp (cdr exp))
      (cond
         ((greaterp (length exp) 2)
            (!&comval
               (list fn2 (car exp) (cons fn1 (cdr exp)))
               status))
         (t (!&call fn2 exp status)))) )
(de !&comtimes (exp status) (!&expandus 'times 'times2 exp status))
(de !&complus (exp status) (!&expandus 'plus 'plus2 exp status))
(de !&commax (exp status) (!&expandus 'max 'max2 exp status))
(de !&commin (exp status) (!&expandus 'min 'min2 exp status))
(put 'plus 'compfn '!&complus)
(put 'times 'compfn '!&comtimes)
(put 'max 'compfn '!&commax)
(put 'min 'compfn '!&commin)
(setq !*r2i t)
(put 'car 'anyreg t)
(put 'cdr 'anyreg t)
(de comrpla (exp status)
   (prog (u v w)
      (setq w (car exp))
      (setq exp (cdr exp))
      (setq u (car exp))
      (setq v (cadr exp))
      (!&clrregs)
      (cond
         ((!&anyreg u (list v))
            (cond
               ((!&anyreg v nil)
                  (progn
                     (setq u (!&locate u))
                     (setq v (!&locate v))))
               (t (progn
                     (!&comval v 1)
                     (setq u (!&locate u))
                     (setq v (list 1)))) ))
         ((!&anyreg v nil)
            (progn
               (!&comval u 1)
               (setq u (list 1))
               (setq v (!&locate v))))
         (t (progn
               (setq u (!&comlis exp))
               (setq u (!&locate (cadr u)))
               (setq v (list 1)))) )
      (!&attach
         (list (cond ((eq w 'rplaca) '!*rplaca) (t '!*rplacd)) u v))
      (!&attach (list '!*load 1 u))
      (setq regs (cons (cons 1 nil) (cdr regs)))
   ))
(put 'rplaca 'compfn 'comrpla)
(put 'rplacd 'compfn 'comrpla)
(de !*rplaca (u v)
   (progn (output1 nil "car(") (pexp u) (prin2 ") = ") (pexp v)
      (prin2 ";")))
(de !*rplacd (u v)
   (progn (output1 nil "cdr(") (pexp u) (prin2 ") = ") (pexp v)
      (prin2 ";")))
(put '!*jumpnc 'optfn '!*jumpncopt)
(de !*jumpncopt (u)
   (cond
      ((eq (car (cdddar u)) 'atom)
         (rplaca u (list '!*jumpc (cadar u) (caddar u) 'pairp)))
      ((eq (car (cdddar u)) 'pairp)
         (rplaca u (list '!*jumpc (cadar u) (caddar u) 'atom)))
      (t nil)))

(put '!*freerstr 'optfn '!*freerstropt)

(de !*freerstropt (u)
   (cond ((and
            (eq (caadr u) '!*lambind)
            (equal (cadar u) (car (cddadr u))) )
          (rplacw u (cddr u)) )
         (t nil) ) )

(put 'pairp 'comtst '!&testfn)
(put 'null 'comtst '!&testfn)
(put 'atom 'comtst '!&testfn)
(put 'idp 'comtst '!&testfn)
(put 'stringp 'comtst '!&testfn)
(put 'fixp 'comtst '!&testfn)
(put 'floatp 'comtst '!&testfn)
(put 'vectorp 'comtst '!&testfn)
(put 'codep 'comtst '!&testfn)
(put 'numberp 'comtst '!&testfn)
(put 'constantp 'comtst '!&testfn)
(put 'globalp 'comtst '!&testfn)
(put 'fluidp 'comtst '!&testfn)
(put 'functionp 'comtst '!&testfn)
(flag
   '(add1 abs alength and append ascii atom close codep compress
       cond cons xcons expand nconc ncons constantp copy difference
       digit eject eq eqn eval evlis explode fix fixp float floatp
       fluid fluidp function get getv greaterp lessp garbage gensym
       getprop getd global globalp go idp intern length linelength
       list liter lposn max max2 min min2 minus mkvect not null
       numberp oblist or onep orderp pagelength pairp posn prin1
       prin2 princ print prog prog2 progn setpchar quit quote remflag
       remprop rplaca rplacd set rds read readch remd remob return
       reverse reversip select setq signoff signon stringp sub1
       system terpri token unfluid unlink upbv vectorp wrs zerop)
   'onereg)
(flag
   '(assoc delete difference divide equal error list2 plus plus2
       quotient expt flag flagp map mapc mapcan mapcar mapcon mapc2
       maplist member memq open load orderp pair sublis times times2
       remainder logor2 logand2 logxor2)
   'tworeg)
(setq compf
'(abs add1 alength and append apply ascii assoc atom caaaar caaadr
caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr
caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar
cddadr cddar cdddar cddddr cdddr cddr cdr close cdif cmod cplus
crecip ctimes setmod codep compress cond cons constantp copy
de deflist delete df difference digit divide dm dump eject eq eqn
equal error errorset eval evlis expand explode expt fix fixp flag
flagp float floatp fluid fluidp function garbage gensym get getprop
getd getv global globalp go greaterp idp intern length lessp linelength
list list2 list3 list4 list5 liter load logor2 logand2 logxor2
lposn map mapc mapc2 mapcan mapcar mapcon maplist max max2 member
memq min min2 minus minusp mkvect nconc ncons not null numberp
oblist open onep or orderp pagelength pair pairp plus plus2 posn
prin1 prin2 princ print prog prog2 progn put putd putv quit quote
quotient rds read readch remainder remd remflag remob remprop
return reverse reversip rplaca rplacd sassoc select set setpchar
setq signoff signon standard!-lisp stringp sub1 sublis subst
system terpri time times times2 token traceable unfluid unlink
upbv vectorp wrs xcons zerop ))

(setq !*nolinke t)

(de !&comcons1 (exp status)
   (prog (u v)
      (cond
         ((or (null (setq exp (cdr exp))) (null (cdr exp)))
            (lprie "mismatch of arguments"))
         ((equal (cadr exp) '(quote nil))
            (return (!&call 'ncons (ncons (car exp)) status))))
      (setq u (car exp))
      (setq v (cadr exp))
      (!&clrregs)
      (cond
         ((!&anyreg u (list v))
            (cond
               ((!&anyreg v nil)
                  (progn
                     (setq u (!&locate u))
                     (setq v (!&locate v))))
               (t (progn
                     (!&comval v 1)
                     (setq u (!&locate u))
                     (setq v (list 1)))) ))
         ((!&anyreg v nil)
            (progn
               (!&comval u 1)
               (setq u (list 1))
               (setq v (!&locate v))))
         (t (progn
               (setq u (!&comlis exp))
               (setq u (!&locate (cadr u)))
               (setq v (list 1)))) )
      (cond
         ((and (equal u '(1)) (equal v '(2)))
            (!&attach (list '!*link 'cons 'expr 2)))
         ((and (equal u '(2)) (equal v '(1)))
            (!&attach (list '!*link 'xcons 'expr 2)))
         ((equal u '(1)) (!&attach (list '!*zcons2 v)))
         ((equal v '(1)) (!&attach (list '!*zcons3 u)))
         (t (!&attach (list '!*zcons u v))))
      (setq regs (cons (ncons 1) (cdr regs)))
nil   ))
(de !*zcons (u v)
   (progn (output1 nil "zconsc(") (pexp u) (prin2 ",") (pexp v)
      (prin2 ");")))
(de !*zcons2 (v)
   (progn (output1 nil "zcons2c(") (pexp v) (prin2 ");")))
(de !*zcons3 (u)
   (progn (output1 nil "zcons3c(") (pexp u) (prin2 ");")))
(put 'cons 'compfn '!&comcons1)


(global
   '(inchannel outchannel !&!&tncp !*traceable !*rdable compfloc
     codechannel evalchannel urwlchannel xrefchannel namechannel
     max!-comp!-size))

(de informuser (x)
   (prog (curchannel)
      (setq curchannel (wrs outchannel))
      (prin2 x)
      (wrs curchannel)))

(setq max!-comp!-size 6000)

(de compilefile (infile outfile)
   (prog (sexpr name type body ncp nfiles  argno xx)
      (setq !&!&tncp (sub1 (length compf)))
      (cond ((or (null outfile) (null infile))
         (return "IO files should be defined!")))
      (setq calledfloc nil)
      (setq body (explode outfile))
      (setq ncp 0)
      (setq name (compress (append body '(c 1))))
      (setq codechannel (open name 'output))
      (output1 "Code files will be named :" name t)
      (setq name (compress (append body '(e))))
      (setq evalchannel (open name 'output))
      (output1 "Evaluate file will be named :" name t)
      (setq name (compress (append body '(u))))
      (setq urwlchannel (open name 'output))
      (output1 "Urwelt file will be named :" name t)
      (setq name (compress (append body '(n 1))))
      (setq namechannel (open name 'output))
      (output1 "Fn-names files will be named :" name t)
      (setq name (compress (append body '(x 1))))
      (setq xrefchannel (open name 'output))
      (output1 "Xref files will be named :" name t)
      (cond (!*traceable (rplacd (getd '!*exit) (cdr (getd '!*exit2))))
            (t (rplacd (getd '!*exit) (cdr (getd '!*exit3)))) )
      (setq !*nolinke !*traceable)
      (setq nfiles 2)
      (setq inchannel (open infile 'input))
      (setq outchannel (wrs nil))
      (wrs outchannel)
loop1 (rds inchannel)
loop  (cond
       (genfunction
         (progn
            (setq sexpr (car genfunction))
            (setq genfunction (cdr genfunction))))
      (t (setq sexpr (read))))
      (cond
        ((and (pairp sexpr) (memq (car sexpr) '(de df))
            (null (flagp (cadr sexpr) 'nocomp)))
         (prog nil
            (setq name (cadr sexpr))
            (wrs outchannel)
            (cond
               ((flagp name 'lose)
                  (progn
                     (output1 t "*** not compiled (Lose flaged) " name t)
                     (return nil)))
               ((memq name compf)
                  (progn
                     (output1
                        t "*** Already compiled function IGNORED -->" name t)
                     (return nil))))
            (output1 t "compiling " name "  ")
            (cond (outchannel
                    (progn (wrs nil)
                           (output1 t "compiling  " name t)
                           (wrs outchannel))))
            (cond
               ((setq xx (assoc name calledf))
                (setq calledf (delete xx calledf))))
            (setq compf (cons name compf))
            (setq compfloc (cons name compfloc))
            (setq !&!&tncp (add1 !&!&tncp))
            (setq argno (length (caddr sexpr)))
            (setq type
               (cdr
                  (assoc (car sexpr) '((de . expr) (df . fexpr)))) )
            (cond ((eq type 'fexpr) (put name 'cfntype '(fexpr))))
            (setq sexpr
               (!&comproc
                  (list3 'lambda (caddr sexpr) (cadddr sexpr)) name))
            (cond (!*traceable
                   (progn
                     (setq sexpr (cons (list3 '!*tracearg !&!&tncp argno) sexpr))
                     (nconc (last sexpr) (ncons !&!&tncp))))
                  (t (progn
                       (cond ((eqcar (setq xx (last sexpr)) '!*linke)
                              (nconc sexpr (ncons '(!*exit1))))
                             ((not (equal xx  '(!*exit)))
                              (nconc sexpr (ncons '(!*exit)))))
                       (cond ((or !*rdable (flagp name 'rdable))
                               (setq sexpr
                                 (cons (list3 '!*redefine !&!&tncp argno) sexpr))
                             )) )))
            (setq sexpr
               (cond
                  ((eq type 'expr)
                     (cons (list4 '!*entry name 'subr 1) sexpr))
                  ((eq type 'fexpr)
                     (cons (list4 '!*entry name 'fsubr 1) sexpr))))
            (cond (!*plap (progn (wrs outchannel) (print sexpr))))
            (setq ncp (plus2 ncp (length sexpr)))
            (wrs codechannel)
            (lap sexpr)
            (priname name argno type)
            (cond
               ((greaterp ncp max!-comp!-size)
                  (progn
                     (wrs xrefchannel)
                     (dumpxtrn)
                     (wrs outchannel)
                     (close codechannel)
                     (close namechannel)
                     (close xrefchannel)
                     (setq name
                        (compress
                           (append
                              body
                              (append '(x) (explode nfiles)))) )
                     (setq xrefchannel (open name 'output))
                     (setq ncp 0)
                     (setq name
                        (compress
                           (append
                              body
                              (append '(c) (explode nfiles)))) )
                     (setq codechannel (open name 'output))
                     (setq name
                        (compress
                           (append
                              body
                              (append '(n) (explode nfiles)))) )
                     (setq namechannel (open name 'output))
                     (setq nfiles (add1 nfiles)))) )
      nil   ))
      ((eqcar sexpr 'open)
         (progn
            (close inchannel)
            (setq inchannel (eval sexpr))
            (go loop1)))
      ((eq sexpr 'end) (return (finalize)))
      ((eq sexpr !$eof!$)
       (progn (wrs nil) (rds nil) (terpri)
              (prin2 "enter file name string or 'end' >")
              (setq sexpr (read))
              (close inchannel)
              (cond ((eq sexpr 'end) (return (finalize)))
                    (t
                      (progn
                         (setq inchannel (open sexpr 'input))
                         (go loop1))  ))))
      (t (prog nil
            (cond
               ((and sexpr (null (flagp (car sexpr) 'ignore)))
                  (progn (wrs evalchannel) (printq sexpr) (wrs outchannel))))
            (cond
               ((and
                   (pairp sexpr)
                   (or (flagp (car sexpr) 'eval)
                       (flagp (car sexpr) 'ignore)))
                 (eval sexpr)))
      nil   )))
      (go loop)))

(de dumpxtrn nil
   (prog (n x y)
      (cond ((null calledfloc) (return nil)))
loop1 (cond ((null (memq (setq y (car calledfloc)) compfloc))
             (setq x (cons y x))))
      (setq calledfloc (cdr calledfloc))
      (cond (calledfloc (go loop1)))
      (setq compfloc nil)
      (cond ((null x) (return nil)))
      (setq n 0)
loop  (setq n (add1 n))
      (cond ((eqn (remainder n 6) 0) (terpri)))
      (prin1 (ersinize (car x)))
      (setq x (cdr x))
      (cond (x (progn (prin2 "(),") (go loop))))
      (prin2 "();") ))

(flag '(fluid global dm putd) 'eval)

(de finalize nil
   (prog (type pname narg)
loop  (cond ((null calledf) (go endofit)))
      (wrs outchannel)
      (output1 t "called but not defined: " (car calledf) t)
      (setq !&!&tncp (add1 !&!&tncp))
      (setq pname (car calledf))
      (setq type (cadr pname))
      (setq narg (caddr pname))
      (setq pname (car pname))
      (wrs codechannel)
      (!*entry pname type 1)
      (output1 " zundefined(" !&!&tncp "," narg "); }" )
      (priname pname narg type)
      (setq calledf (cdr calledf))
      (setq compfloc (cons pname compfloc))
      (go loop)
endofit
      (setpchar ">")
      (wrs urwlchannel)
      (print (add1 urlength))
      (setq urwelt (reversip urwelt))
      (mapc urwelt (function print))
      (print nil)
    %  (rds nil)
      (wrs xrefchannel)
      (dumpxtrn)
      (wrs outchannel)
      (close codechannel)
      (close evalchannel)
      (close urwlchannel)
      (close namechannel)
      (close inchannel)
      (close xrefchannel)
      (return "compilation finished")))

(de spaces (n)
   (prog (i)
      (setq i 1)
!$label
      (cond ((greaterp i n) (return nil)))
      (prin2 " ")
      (setq i (add1 i))
      (go !$label)))

(fluid '(!&u !&v))
(de !*tracearg (!&u !&v)
     (output1 t "#if TRACEABLE"
              nil "if(ztracearg(" !&u "," !&v ")) goto exit;" t "#endif") )

(de !*redefine (!&u !&v)
     (output1 t "#if TRACEABLE"
              nil "if(zredefined(" !&u "," !&v ")) return;" t "#endif") )

(de !*exit2 (!&u) (output1 t "#if TRACEABLE" t "exit:" nil "ztraceval(" !&u
                           ");" t "#endif" t "return;" t "}" t) )
(de !*exit1 nil (output1 nil "}" t))
(de !*exit3 nil (output1 nil "return;" t "}" t))
(de !*exit nil (!*exit3))                    %dummy routine.

(de priname (nm n type)
   (prog (u v)
      (wrs namechannel)
      (prin2 nm)
      (setq v (length (del '!! (explode nm))))
      (cond ((lessp v 21)
             (spaces (difference 21 v)))
            (t   (spaces 1)))
      (setq v (length (explode (prin2 (ersinize nm)))))
      (cond ((lessp v 21)
             (spaces (difference 21 v) ))
            (t   (spaces 1)))
      (cond
        ((eq type 'expr)
             (progn (prin2 n) (prin2 "  subr")))
        (t (prin2 "1  fsubr")))
      (terpri)
      (wrs outchannel) ))

(setq !*plap nil)

(flag '(remove) 'specialname)

(flag '(zerop reversip) 'lose)

(de mkprog (u v) (cons 'prog (cons u v)))

(de quotep1 (x)
   (and
      (eqcar x 'quote)
      (not (atom (cdr x)))
      (null (cddr x))))


(de prin1q (x)
   (cond
      ((atom x) (prin1 x))
      ((quotep1 x) (progn (prin2 '!') (prin1q (cadr x)) ) )
      (t (prog nil
            (prin2 '!( )
      l1    (prin1q (car x))
            (setq x (cdr x))
            (cond ((pairp x) (progn (prin2 '!  ) (go l1)))
                  (x (progn (prin2 " . ") (prin1 x))))
            (prin2 '!) )))))

(de printq (x) (progn (prin1q x) (terpri)))
(output1 "compiler loaded." t "usage: (compilefile <infile> <outfile>)" t)
