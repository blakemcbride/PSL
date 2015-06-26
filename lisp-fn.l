/*  Warning:  DO NOT use $ or @ characters in this file.
    They do have some special meanings */
void booleanp(i)      /* utility used by most of boolean functions */
int i;
{ reg1 = i ? T : NIL; }

int greaterp1(x,y)    /* utility used by Greaterp, Lessp */
PSEXP x,y;
{ switch(type(x)){
    case Tinteger:
       switch(type(y)){
         case Tinteger: if(intval(x) > intval(y)) goto jt; else goto jn;
         case Tbig:     if(bigval(y) < 0) goto jt; else goto jn;
         case Tfloating:if(intval(x) > floval(y)) goto jt; else goto jn;
         default:       return(2); }
    case Tfloating:
       switch(type(y)){
         case Tinteger: if(floval(x) > intval(y)) goto jt; else goto jn;
         case Tbig:     if(floval(x) > zbig2float(y)) goto jt; else goto jn;
         case Tfloating:if(floval(x) > floval(y)) goto jt; else goto jn;
         default:       return(2); }
    case Tbig:
       switch(type(y)){
         case Tinteger: if(bigval(x) > 0) goto jt; else goto jn;
         case Tbig:
           if(bigval(x) > bigval(y)) goto jt;
           else if(bigval(x) < bigval(y)) goto jn;
           else
            { int s = 1;
              if(bigval(x)<0) s = -1;
              while(!null(bigcdr(x)) && bigval(x) == bigval(y))
                { x = bigcdr(x); y = bigcdr(y); }
              if((s*bigval(x)) > (s*bigval(y))) goto jt; else goto jn;
            }
         case Tfloating: if(zbig2float(x) > floval(y)) goto jt; else goto jn;
         default:        return(2); }
    default: return(1);            }
jt: reg1 = T; return(0);
jn: reg1 = NIL; return(0);
}

int addsub1(n)    /* utility used by Add1, Sub1 */
int n;
{ switch(type(reg1)) {
    case Tinteger:
       znormalize(intval(reg1) + n);
       return(0);
    case Tbig :
       kalloc(1);
       local0 = reg2; reg2 = Sexp(&ONE);
       zaddsub(n); reg2 = local0;
       kpop(1);
       return(0);
    case Tfloating :
       reg1 = zfloating(floval(reg1) + n);
       return(0);
    default : return(1);  }
}

int addsub(s)     /* utility used by Plus2, Difference */
int s;
{ double d;
  switch(type(reg1)){
    case Tinteger:
       switch(type(reg2)){
         case Tinteger:  znormalize(intval(reg1) + s*intval(reg2));
                         return(0);
         case Tbig:      zaddsub(s); return(0);
         case Tfloating: d = s*floval(reg2) + intval(reg1); goto dbl;
         default:        return(2); }
    case Tfloating:
       d = floval(reg1);
       switch(type(reg1)){
         case Tinteger:  d += s*intval(reg2); goto dbl;
         case Tbig:      d += s*zbig2float(reg2); goto dbl;
         case Tfloating: d += s*floval(reg2); goto dbl;
         default:        return(2); }
    case Tbig:
       switch(type(reg1)){
         case Tinteger:
         case Tbig:      zaddsub(s); return(0);
         case Tfloating: d = zbig2float(reg1) + s*floval(reg2); goto dbl;
         default:        return(2); }
    default:   return(1);   }
dbl: reg1 = zfloating(d); return(0);
}

void Abs()
{ float y;
  long l;
  switch(type(reg1)) {
    case Tinteger:
       if((l = intval(reg1)) < 0) reg1 = zinteger(-l);
       return;
    case Tbig :
       if((l = bigval(reg1)) < 0) reg1 = zbig(-l,bigcdr(reg1));
       return;
    case Tfloating :
       if((y = floval(reg1)) < 0.0) reg1 = zfloating(-y);
       return;
    default : zerror(12, reg1, @); }
}
/*@*/
Add1()
{ if(addsub1(1)) zerror(12, reg1, @);
}
/*@*/
Alength()
{ int l,n;
  switch(type(reg1)){
    case Tpair:
    case Tvector: reg1 = NIL; return;
    case Tbig:
      l = (int)bigval(reg1);     /* # of cells */
      n = l<0 ? 1 : 0;           /* sign character */
      if(l<0) l = -l;
      sprintf(tx,"%ld",bigval(bigcdr(reg1)));
      reg1 = zinteger((long) (n + (l-1)*DZ + strlen(tx)));
      return;
    default:
      zescape(reg1);
      reg1 = zinteger((long) strlen(tx));
   }
}
/*@*/
And()
{
  if(null(reg1)) zreturn(T)
  kalloc(1);
  while(1)
    { if(null(cdr(reg1))) { reg1 = car(reg1); Eval();
                            kpop(1); return; }
      kset0(reg1);
      reg1 = car(reg1); Eval();
      if(null(reg1)) zkreturn(NIL,1)    /* kpop(1); zreturn(NIL); */
      reg1 = cdr(local0);
    }
}
/*@*/
Append()
{
  if(null(reg1)) zreturn(reg2)
  if(atom(reg1)) zerror(0, reg1, Sexp(&npair), @);
  kalloc(2);
  kset0(cdr(reg1));
  reg1 = local(1) = zcons1(car(reg1));
  while(!null(local0))
    { if(atom(local0)) zerror(0, local0, Sexp(&npair), @);  /* 6.0 TY */
      reg1 = cdr(reg1) = zcons1(car(local0));
      local0 = cdr(local0); }
  cdr(reg1) = reg2;
  kload(reg1,1);
  kpop(2);
}
/*@*/
Apply()
{
  register PSEXP f, u;
  int n, i;

#if TRACEABLE
  if(ztracearg($,2)) goto exit;
#endif
  if (codep(f=reg1))
    { u = reg2;
      n = argno(f) - 1;
      if (n > 0)
        { kalloc(n);
          saveregisters(n);
        }
      for (i = 0; i <= n; i++)
        { if (null(u))
            zerror(17);
          registers[i] = car(u);
          u = cdr(u);
        }
      (fnc(f))();
      if (n > 0)
        { restoreregisters(n);
          kpop(n);
        }
#if TRACEABLE
      ztraceval($);
#endif
      return;
    }
  if (idp(f))
    { if (!functionp(f))
        zerror(15, f);
      else if (car(value(f)) == Sexp(&expr) || car(value(f)) == Sexp(&subr))
        { reg1 = cdr(value(f));
          Apply();
#if TRACEABLE
          ztraceval($);
#endif
          return;
        }
      else
        zerror(16, f);
    }
  if (!pairp(f) || car(f) != Sexp(&lambda))
    zerror(16, f);
  u = cadr(f);
  n = 0;
  while (u != NIL)
    { if (null(reg2)) zerror(17);
      zalist(car(u), value(car(u)));
      value(car(u)) = car(reg2);
      u = cdr(u);
      reg2 = cdr(reg2);
      n++;
    }
  if (reg2 != NIL)
    zerror(17);
  reg1 = caddr(f);
  Eval();
  while (n--) zpopalist();
#if TRACEABLE
exit:  ztraceval($);
#endif
  return;
}

/*@*/
Ascii()
{
  if(fixp(reg1)) zreturn(ch(intval(reg1) % 128))
  zerror(0, reg1, Sexp(&ninteger), @);
}
/*@*/
Assoc()
{
  PSEXP u,v;
  u = reg1; v = reg2;
  while(1)
    { if(null(v)) zreturn(NIL)
      if(atom(v) || atom(car(v))) zerror(26, reg2);
      reg2 = caar(v); reg1 = u; Equal();
      if(reg1 != NIL) zreturn(car(v))
      v = cdr(v);
    }
}
/*@*/
Atom()
{
  booleanp(!pairp(reg1));
}
/*@*/
Caaaar()
{
  Caar(); Caar();
}
/*@*/
Caaadr()
{
  Cdr(); Caaar();
}
/*@*/
Caaar()
{
  Caar(); Car();
}
/*@*/
Caadar()
{
  Cadar(); Car();
}
/*@*/
Caaddr()
{
  Caddr(); Car();
}
/*@*/
Caadr()
{
  Cadr(); Car();
}
/*@*/
Caar()
{
  Car(); Car();
}
/*@*/
Cadaar()
{
  Cdaar(); Car();
}
/*@*/
Cadadr()
{
  Cdadr(); Car();
}
/*@*/
Cadar()
{
  Cdar(); Car();
}
/*@*/
Caddar()
{
  Cddar(); Car();
}
/*@*/
Cadddr()
{
  Cdddr(); Car();
}
/*@*/
Caddr()
{
  Cddr(); Car();
}
/*@*/
Cadr()
{
  Cdr(); Car();
}
/*@*/
Car()
{
  if(pairp(reg1))
    reg1 = car(reg1);
  else
    zerror(0, reg1, Sexp(&npair), @);
}
/*@*/
Cdaaar()
{
  Caaar(); Cdr();
}
/*@*/
Cdaadr()
{
  Caadr(); Cdr();
}
/*@*/
Cdaar()
{
  Caar(); Cdr();
}

/*@*/
Cdadar()
{
  Cadar(); Cdr();
}
/*@*/
Cdaddr()
{
  Caddr(); Cdr();
}
/*@*/
Cdadr()
{
  Cadr(); Cdr();
}
/*@*/
Cdar()
{
  Car(); Cdr();
}
/*@*/
Cddaar()
{
  Cdaar(); Cdr();
}
/*@*/
Cddadr()
{
  Cdadr(); Cdr();
}
/*@*/
Cddar()
{
  Cdar(); Cdr();
}
/*@*/
Cdddar()
{
  Cddar(); Cdr();
}
/*@*/
Cddddr()
{
  Cdddr(); Cdr();
}
/*@*/
Cdddr()
{
  Cddr(); Cdr();
}
/*@*/
Cddr()
{
  Cdr(); Cdr();
}
/*@*/
Cdr()
{
  if(pairp(reg1))
    reg1 = cdr(reg1);
  else
    zerror(0, reg1, Sexp(&npair), @);
}
/*@*/
Close1()
{ FILE *f;
  if(fixp(reg1)) 
    { fclose( f=(FILE *) intval(reg1) );
      kill_input_buf_save_entry(f); }               /* 7.0 . It will not harm write files */ 
  else zerror(20, reg1);                            /*       Since they have no buf.table */
}                                                   /*       entry at all.                */
/*@*/
Cdif()
{ int i;
  i = addsub(-1);
  if(i == 1) zerror(12, reg1, @);
  else if(i == 2) zerror(12, reg2, @);
  Cmod();
}
/*@*/
Cmod()
{ char x;
  x = type(reg1);
  if(!((x == Tinteger) || (x == Tbig))) zerror(0, reg1, Sexp(&ninteger), @);
  x = type(reg2 = value(&cmod1));
  if(!((x == Tinteger) || (x == Tbig))) zerror(0, reg2, Sexp(&ninteger), @);
  zdivision(0);
}
/*@*/
Cplus()
{ int i;
  i = addsub(1);
  if(i == 1) zerror(12, reg1, @);
  else if(i == 2) zerror(12, reg2, @);
  Cmod();
}
/*@*/
Crecip()
{ long r,a,b,x,y,pr;

  if(!fixp(reg1)) goto err;
  y = intval(reg1);
  if(!fixp(reg1 = value(&cmod1))) goto err;
  pr = x = intval(reg1); a = 1; b = 0;
  while(y>0)
    { r = -(x/y)*a + b; b = a; a = r;
      r = x%y; x = y; y = r; }
  reg1 = zinteger((b>0 ? 0 : pr) + b%pr);
  return;
err:
  zerror(36, reg1, @);
}
/*@*/
Ctimes()
{ char x;
  if(!(((x = type(reg1)) == Tinteger) || (x == Tbig)))
    zerror(0, reg1, Sexp(&ninteger), @);
  if(!(((x = type(reg2)) == Tinteger) || (x == Tbig)))
    zerror(0, reg2, Sexp(&ninteger), @);
  zmultiply();
  Cmod();
}
/*@*/
Setmod()
{ char x;
  PSEXP a;
  a = value(&cmod1);
  if((((x = type(reg1)) == Tinteger) && intval(reg1)>1) || (x == Tbig)
                                                        || null(reg1))
    { value(&cmod1) = reg1; reg1 = a; }
  else zerror(0, reg1, Sexp(&ninteger), @);
}
/*@*/
Codep()
{
  booleanp(codep(reg1));
}
/*@*/
Compress()
{
  int i=0, zycpscheck();
  void zycpsunput();
  PSEXP u;
  char *c;
  c = tx;
  while(!null(reg1))
    { if(i++ == TXSIZE) zerror(1);   /* size of array tx limits */
      if(!pairp(reg1)) zerror(0, reg1, Sexp(&npair), @);
      u = car(reg1);
      if(idp(u))
        { if(pname(u)[1]) zerror(28, u, @);
          *c++ = *pname(u); }
      else if(fixp(u))
        { if(intval(u) < 0 || intval(u) > 9) zerror(28, u, @);
          sprintf(c, "%1ld", intval(u));
          c++; }
      else
        zerror(28, u, @);
      reg1 = cdr(reg1);

    }
  *c = 0;
  zycpsunput(c);
  Token();
  if(zycpscheck()) zerror(1);
}
/*@*/
Cond()
{
  kalloc(1);
  while(!null(reg1))
    { if(atom(reg1) || atom(car(reg1))) zerror(11);
      kset0(reg1); reg1 = caar(reg1); Eval();
      if(!null(reg1)) { reg1 = cadar(local0);
                        Eval(); kpop(1); return; }
      reg1 = cdr(local0);
    }
  kpop(1);
}
/*@*/
Cons()
{
  zcons2c(reg2);
}
/*@*/
Constantp()
{
  booleanp(constantp(reg1));
}
/*@*/
Copy()
{
  if(atom(reg1)) return;
  kalloc(2);
  kset0(cdr(reg1));
  local(1) = reg1 = zcons1(car(reg1));
  while(!null(local0))
    { local(1) = cdr(local(1)) = zcons1(car(local0));
      local0 = cdr(local0); }
  kpop(2);
}

void defn()
{ reg2 = reg1;      /* utility used by de,df,dm */
  Caddr(); Ncons();
  reg3 = zcons(cadr(reg2),reg1);
  reg3 = zcons(Sexp(&lambda),reg3);
  reg1 = car(reg2);
}
/*@*/
De()
{
  defn();
  reg2 = Sexp(&expr);
  Putd();
}
/*@*/
Deflist()    /*GU*/ /*This code is compiler generated, works very well */
{
  if (null(reg1)) return;       /*but not efficient enough TY*/
  kalloc(2);
  kset0(reg1);
  Cadar(); reg3 = reg1;
  kload0(reg1); Caar(); kset(1,reg1);
  Put();
  reg1 = cdr(local0);
  Deflist();
  zcons3c(local(1));
  kpop(2);
}

/*@*/
/* STD-LISP COMPILER COMPILED Delete ... as you see a programm is better then
   T. Yilmaz...................................................Saka, saka */
Delete()
{
  if (null(reg2)) zreturn(NIL)
  if(atom(reg2)) zerror(0, reg2, Sexp(&npair), @);
  kalloc(2);
  kset0(reg1);
  kset(1,reg2);
  reg2 = car(reg2);
  Equal();
  if (null(reg1)) goto lab1;
  reg1 = cdr(local(1));
  goto lab2;
lab1 :
  reg2 = cdr(local(1));
  kload0(reg1);
  Delete();
  zcons3c(car(local(1)));
lab2 :
  kpop(2); }

/*@*/
Df()
{
  defn();
  reg2 = Sexp(&fexpr);
  Putd();
}
/*@*/
Difference()
{ switch(addsub(-1)) {
    case 0: return;
    case 1: zerror(12, reg1, @);
    case 2: zerror(12, reg2, @); }
}
/*@*/
Digit()
{
  booleanp(reg1 >= ch('0') && reg1 <= ch('9'));
}
/*@*/
Divide()
{ char x;
  x = type(reg1);
  if(!((x == Tinteger) || (x == Tbig))) zerror(36,reg1,@);
  x = type(reg2);
  if(!((x == Tinteger) || (x == Tbig))) zerror(36,reg2,@);
  zdivision(2);
}
/*
Dl()
{
  defn();
  reg2 = Sexp(&lexpr);
  Putd();
}   */
/*@*/
Dm()
{
  defn();
  reg2 = Sexp(&macro);
  Putd();
}
/*@*/
Dump()
{ 
 #if DEBUG 
  if(stringp(reg1)) zdump(strbody(reg1));
  else if(idp(reg1)) zdump(pname(reg1));
  else zerror(27, reg1, @);
 #else
  printf("\n*** dump not implemented !\n");
#endif
}
/*
Dn()
{
  defn();
  reg2 = Sexp(&nexpr);
  Putd();
}    */
/*@*/
Eject()
{
  putc('\f', outputf);
  currlin = currcol = 0;
  reg1 = NIL;
}
/*@*/
Eq()
{
  booleanp(reg1 == reg2);
}
/*@*/
Eqn()
{ if(reg1 == reg2) goto jt;
  switch(type(reg1)){
    case Tinteger:
       switch(type(reg2)){
         case Tinteger: if(intval(reg1) == intval(reg2)) goto jt; else goto jn;
         case Tbig:     goto jn;
         case Tfloating:if(intval(reg1) == floval(reg2)) goto jt; else goto jn;
         default:       goto jn; }
    case Tfloating:
       switch(type(reg2)){
         case Tinteger: if(floval(reg1) == intval(reg2)) goto jt; else goto jn;
         case Tbig:     if(floval(reg1) == zbig2float(reg2)) goto jt; else goto jn;
         case Tfloating:if(floval(reg1) == floval(reg2)) goto jt; else goto jn;
         default:       goto jn; }
    case Tbig:
       switch(type(reg2)){
         case Tinteger: goto jn;
         case Tbig:     if(bigval(reg1) != bigval(reg2)) goto jn;
                        else
                          { 
                            while(!null(reg1 = bigcdr(reg1))
                                  && !null(reg2 = bigcdr(reg2))) 
                              { reg1 = bigcdr(reg1);
                                reg2 = bigcdr(reg2);
                                if(bigval(reg1) != bigval(reg2)) goto jn;
                              }
                            goto jt; }
         case Tfloating: if(zbig2float(reg1) == floval(reg2)) goto jt; else goto jn;
         default:        goto jn; }
    default: goto jn;            }
jt: reg1 = T; return;
jn: reg1 = NIL;
}
/*@*/
Equal()
{
  PSEXP u, v;

  while(1)
    { if(reg1 == reg2) goto jt;
      u = reg1;
      Eqn();                   /* returns either NIL or T */
      if(reg1 == T) return;
      if(type(u) != type(reg2)) return; /* reg1 is NIL already */
      v = reg2;
      switch(type(reg2))
        {
          case Tpair : reg1 = car(u); reg2 = car(reg2); Equal();
                       if(null(reg1)) return; /* reg1 is NIL already */
                       reg1 = cdr(u);
                       reg2 = cdr(v);
                       break;
          case Tstring : booleanp(!strcmp(strbody(u), strbody(reg2))); return;
          case Tvector : if (upbv(u) != upbv(reg2)) goto jn;
                         { unsigned int n;
                           for(n=0; n<=upbv(reg1); n++)
                             { reg1 = vectelts(u)[n]; reg2 = vectelts(v)[n];
                               Equal();
                               if(null(reg1)) goto jn;
                             }
                          }
                          goto jt;
          default : goto jn;
        }
    }
  jt: reg1 = T; return;
  jn: reg1 = NIL;
}
/*@*/
Error()
{
  PALISTENT als;
  int n;

  n = (int) intval(reg1);
  reg1 = reg2;
  if(currcol)
    { putc('\n', outputf);
      cnewline;
      currcol = 0;
    }
  if(curtrap->msgprint)
    { fprintf(outputf, " ***** ");
      currcol = 6;
      if(atom(reg1)) Prin2();
      else
        while(1)
          { reg1 = car(reg2); Prin2();
            reg2 = cdr(reg2);
            if(null(reg2)) break;
            zedit(" ");
          }
      putc('\n', outputf);
      cnewline;
      currcol = 0;
    }
  if(curtrap->backtrace) dispstackarg();
  als = curtrap->curalist;
  while(als <alisttop) zpopalist();
  zstackptr = curtrap->stacksave;
  longjmp(curtrap->errsave, n);
}
/*@*/
Errorset()
{
  int errnumber;

  curtrap++;
  curtrap->msgprint = (null(reg2) ? 0 : 1);
  curtrap->backtrace = (null(reg3) ? 0 : 1);
  curtrap->curalist = alisttop;
  if((curtrap->stacksave = zstackptr, errnumber = setjmp(curtrap->errsave)))
    { curtrap--;
      reg1 = emsg.Xvalue = zinteger((long) errnumber);
      return;
    }
  Eval();
  curtrap--;
  Ncons();
}
/*@*/
Eval()
{
  if(constantp(reg1)) return;
#if TRACEABLE
  if(ztracearg($,1)) goto exit;
#endif
  if(idp(reg1))
    if(value(reg1) == NULL || functionp(reg1))
      zerror(18, reg1);
    else
      { reg1 = value(reg1);
#if TRACEABLE
        ztraceval($);
#endif
        return; }
  kalloc(3); kset0(reg1); kset(1,reg2);
  if(pairp(car(reg1)))
    if(caar(reg1) == Sexp(&lambda)) goto lab1;
      else zerror(19, car(reg1));
  if(codep(car(reg1))) goto lab1;
  if(!idp(car(reg1)) || !functionp(car(reg1))) zerror(15, car(reg1));
  reg1 = value(car(reg1));
  if(car(reg1) == Sexp(&expr) || car(reg1) == Sexp(&subr))
    { kset(2,reg1); reg1 = cdr(local0); Evlis(); reg2 = reg1;
      reg1 = cdr(local(2)); Apply();
      goto end; }
  else if(car(reg1) == Sexp(&fexpr) || car(reg1) == Sexp(&fsubr))
    { dummypair.Xcar = cdr(local0);
      reg1 = cdr(reg1); reg2 = Sexp(&dummypair); Apply();
      goto end; }
/*else if(car(reg1) == Sexp(&lexpr) || car(reg1) == Sexp(&lsubr))
    { kset(2,reg1);
      reg1 = cdr(local0); Evlis();
      dummypair.Xcar = reg1;
      reg1 = cdr(local(2)); reg2 = Sexp(&dummypair); Apply();
      goto end; }
  else if(car(reg1) == Sexp(&nexpr) || car(reg1) == Sexp(&nsubr))
    { reg1 = cdr(reg1); reg2 = cdr(local0); Apply();
      goto end; }  */
  else
    { dummypair.Xcar = local0;
      reg1 = cdr(reg1); reg2 = Sexp(&dummypair); Apply(); Eval();
      goto end; }
lab1:
  reg1 = cdr(reg1); Evlis();  reg2 = reg1;
  reg1 = car(local0); Apply();
end: kload(reg2, 1);kpop(3);
#if TRACEABLE
exit: ztraceval($);
#endif
}
/*@*/
Evlis()
{
  if(null(reg1)) return;
  if(atom(reg1)) zerror(0, reg1, Sexp(&npair), @);
#if TRACEABLE
  if(ztracearg($,1)) goto exit;
#endif
  kalloc(3); kset0(cdr(reg1));
  reg1 = car(reg1);
  Eval();
  local(2) = local(1) = zcons1(reg1);
  while(!null(local0))
    { reg1 = car(local0); Eval();
      local(2) = cdr(local(2)) = zcons1(reg1);
      kset0(cdr(local0));
    }
  kload(reg1, 1);  kpop(3);
#if TRACEABLE
exit: ztraceval($);
#endif
}
/*@*/
Expand()
{
  if(atom(reg1)) return;
  if(null(cdr(reg1))) zreturn(car(reg1))
  kalloc(1);
  kset0(reg1);
  reg1 = cdr(reg1); Expand();
  Ncons();
  zcons3c(car(local0));
  Xcons();
  kpop(1);
}
/*@*/
Explode()
{ char *x;
  int n;

  kalloc(1);
  if(bigp(reg1))
    { n = (int)bigval(reg1);
      if(abs(n) > 100) zerror(36,reg1,@);   /* max. 100*DZ length list allowed */
      reg1 = bigcdr(reg1);
      x = tx;
      if(n<0) *x++ = '-';
      sprintf(x, "%ld", bigval(reg1));
      x = tx;
      reg2 = local0 = zcons1(Sexp(ch(*x++)));
      while(1)
        { while(*x) local0 = cdr(local0) = zcons1(Sexp(ch(*x++)));
          if(null(reg1 = bigcdr(reg1))) break;
          sprintf(tx, "%ld", bigval(reg1));
          n = strlen(tx);
          while(n++ < DZ) local0 = cdr(local0) = zcons1(Sexp(ch('0')));
          x = tx;
        }
      zkreturn(reg2,1)
    }
  if(pairp(reg1) || vectorp(reg1)) zerror(0, reg1, Sexp(&nid), @);
  zescape(reg1);
  x = tx;
  local0 = reg1 = zcons1(Sexp(ch(*x++)));
  while(*x) local0 = cdr(local0) = zcons1(Sexp(ch(*x++)));
  kpop(1);
}
/*@*/
Expt()
{ switch(type(reg1)){
    case Tinteger:
       switch(type(reg2)){
         case Tinteger:
         case Tbig:     zexpt(); return;
         case Tfloating:
            reg1 =  zfloating(pow((double)intval(reg1), floval(reg2)));
            return;
         default:       goto df; }
    case Tfloating:
       switch(type(reg2)){
         case Tinteger:
            reg1 = zfloating(pow(floval(reg1), (double)intval(reg2)));
            return;
         case Tbig:
            reg1 = zfloating(pow(floval(reg1), zbig2float(reg2)));
            return;
         case Tfloating:
            reg1 = zfloating(pow(floval(reg1), floval(reg2)));
            return;
         default:       goto df; }
    case Tbig:
       switch(type(reg2)){
         case Tinteger:
         case Tbig:     zexpt(); return;
         case Tfloating:
            reg1 = zfloating(pow(zbig2float(reg1), floval(reg2)));
            return;
         default:       goto df; }
    default:  zerror(12, reg1, @); }
df:   zerror(12, reg2, @);
}

/*@*/
Fix()
{ int i,j=0,s;
  double f,z;
  long k;
  switch(type(reg1)){
    case Tinteger :
    case Tbig     : return;
    case Tfloating:
      f = floval(reg1);
      z = fabs(f);
      if(z < 1.0) reg1 = Sexp(&ZERO);
      else if(z <= BFT) zfnormalize(f);
      else
        { i = (int)log10(f);
          s = f<0 ? -1 : 1;
          reg1 = NIL;
          while(i>=2*DZ) { reg1 = zbig(0L,reg1); j++; i -= DZ; }
#if _TURBOC_
          z /= pow10(j*DZ);
#else
          z /= pow(10,j*DZ);
#endif
          k = (long)(z/BASE);
          z -= k*BASE;
          reg1 = zbig((long)z,reg1); j++;
          if(k) { reg1 = zbig(k,reg1); j++; }
          reg1 = zbig((long)s*j, reg1); }
      return;
    default: zerror(12, reg1, @); }
}
/*@*/
Fixp()
{ char i;
  i = type(reg1);
  booleanp((i == Tinteger) || (i == Tbig));
}
/*@*/
Flag()
{ PSEXP u,v,z;

  if(!idp(reg2)) zerror(0, reg2, Sexp(&nid), @);
  while(!null(reg1))
    { if(!pairp(reg1)) zerror(0, reg1, Sexp(&npair), @);
      if(!idp(z = car(reg1))) zerror(0, car(reg1), Sexp(&nid), @);
      u = v = proplist(z);
      while(!null(u))
        { if(reg2 == car(u)) goto found;
          u = cdr(u); }
      kalloc(2);         /* necessary for compactified garbage collection TY */
      kset0(z);  kset(1,v);
      proplist(local0) = zcons(reg2, local(1));
      kpop(2);
  found:
      reg1 = cdr(reg1);
    }
}
/*@*/
Flagp()
{  PSEXP z;

   if(!idp(reg1)) goto jnil;
   z = proplist(reg1);
   while(!null(z))
     { if(car(z) == reg2) zreturn(T)
       z = cdr(z); }
jnil:
   zreturn(NIL)
}
/*@*/
Float()
{ double x;
  switch(type(reg1)){
    case Tinteger:   x = (double) intval(reg1); break;
    case Tfloating:  return;
    case Tbig:       x = zbig2float(reg1); break;
    default:         zerror(12, reg1, @); }
  reg1 = zfloating(x);
}
/*@*/
Floatp()
{
  booleanp(floatp(reg1));
}
/*@*/
Fluid()
{
  while(!null(reg1))
    { if(!pairp(reg1)) zerror(0, reg1, Sexp(&npair), @);
      if(!idp(car(reg1))) zerror(0, car(reg1), Sexp(&nid), @);
      if(fluidp(car(reg1)))
        { reg1 = cdr(reg1);
          continue;
        }
      if(globalp(car(reg1)) || functionp(car(reg1))) zerror(3, car(reg1));
#if BITF
      fluidp(car(reg1)) = 1;
#else
      attribute(car(reg1)) |= 0x08;
#endif
      value(car(reg1)) = NIL;
      reg1 = cdr(reg1);
    }
}
/*@*/
Fluidp()
{
  booleanp(fluidp(reg1));
}
/*@*/
Function()
{
  Quote();
}
/*@*/
Garbage()
{  reg1 = NIL;
   zgarbage(Tsname+1);
}
/*@*/
Gensym()
{/* Radical change in ver 3.3: from now on gensym's are
    in the hash bucket like an usual atom*/
  sprintf(tx, "g%d", gensymcounter++);
  zintern(0);                 /* zintern(0) makes inoblistp( ) = 0; */
}
/*@*/
Get()
{
  if(!idp(reg1)) zreturn(NIL)
  reg1 = proplist(reg1);
  while(!null(reg1))
    { if(pairp(car(reg1)) && caar(reg1) == reg2) zreturn(cdar(reg1))
      reg1 = cdr(reg1); }
  reg1 = NIL;
}
/*@*/
Getd()
{
  reg1 = !idp(reg1) ? NIL : functionp(reg1) ? value(reg1) : NIL;
}
/*@*/
Getprop()
/*  accesses all the prop. list.  */
{
  if(!idp(reg1)) zerror(0, reg1, Sexp(&nid), @);
  reg1 = proplist(reg1);
}
/*@*/
Getv()
{ long x;
  if(!vectorp(reg1)) zerror(0, reg1, Sexp(&nvector), @);
  if(!fixp(reg2)) zerror(0, reg2, Sexp(&ninteger), @);
  if((x = intval(reg2)) < 0 || x > upbv(reg1)) zerror(9, reg2);
  reg1 = vectelt(reg1, x);
}
/*@*/
Global()
{ PSEXP z;
  while(!null(reg1))
    { if(!pairp(reg1)) zerror(0, reg1, Sexp(&npair), @);
      if(!idp(z = car(reg1))) zerror(0, z, Sexp(&nid), @);
      if(globalp(z) || functionp(z)) { reg1 = cdr(reg1); continue; }
      if(fluidp(z)) zerror(4, z);
#if BITF
      globalp(z) = 1;
#else
      attribute(z) |= 0x10;
#endif
      value(z) = NIL;
      reg1 = cdr(reg1);
    }
}
/*@*/
Globalp()
{
#if BITF
  booleanp(globalp(reg1) || functionp(reg1));
#else
  booleanp(attribute(reg1) & 0x14);
#endif
}
/*@*/
Go()
{
  forprog.Xerrorno = 2;
  forprog.Xerrormsg = car(reg1);
  reg1 = Sexp(&forprog);
}
/*@*/
Greaterp()
{ switch(greaterp1(reg1,reg2)) {
    case 0: return;
    case 1: zerror(12, reg1, @);
    case 2: zerror(12, reg2, @); }
}
/*@*/
Idp()
{
  booleanp(idp(reg1));
}
/*@*/
Intern()     /*Changed ver 3.3*/
{
  if(!idp(reg1)) zerror(0, reg1, Sexp(&nid), @);
#if BITF
  inoblistp(reg1) = 1;
#else
  attribute(reg1) |= 0x02;
#endif
}
/*@*/
Length()
{
  long int n;

  n = 0;
  while(1)
    { if(atom(reg1)) zreturn(zinteger(n))
      reg1 = cdr(reg1);
      n++;
    }
}
/*@*/
Lessp()
{ switch(greaterp1(reg2,reg1)) {
    case 0: return;
    case 1: zerror(12, reg2, @);
    case 2: zerror(12, reg1, @); }
}
/*@*/
Linelength()
{
  if(fixp(reg1) && intval(reg1) > 0)
    { lineln = intval(reg1); return; }
  if(null(reg1)) zreturn(zinteger((long) lineln))
  zerror(21, reg1);
}
/*@*/
List()
{
  Evlis();
}
/*@*/
List2()
{
  reg2 = zcons1(reg2);
  zcons2c(reg2);
}
/*@*/
List3()
{
  reg3 = zcons1(reg3);
  reg2 = zcons(reg2, reg3);
  zcons2c(reg2);
}
/*@*/
List4()
{
  reg4 = zcons1(reg4);
  reg3 = zcons(reg3, reg4);
  reg2 = zcons(reg2, reg3);
  zcons2c(reg2);
}
/*@*/
List5()
{
  reg5 = zcons1(reg5);
  reg4 = zcons(reg4, reg5);
  reg3 = zcons(reg3, reg4);
  reg2 = zcons(reg2, reg3);
  zcons2c(reg2);
}
/*@*/
Liter()
{
  booleanp(reg1 >= ch('a') && reg1 <= ch('z') ||
          reg1 >= ch('A') && reg1 <= ch('Z'));
}
/*@*/
Load()
{
  reg2 = Sexp(&inpt);
  Open1();
  Rds();
}
/*@*/
Logor2()
{
  if(fixp(reg1) && fixp(reg2)) reg1 = zinteger(intval(reg1) | intval(reg2));
  else if(!fixp(reg1)) zerror(39,reg1,@);
  else zerror(39,reg2,@);
}
/*@*/
Logand2()
{
  if(fixp(reg1) && fixp(reg2)) reg1 = zinteger(intval(reg1) & intval(reg2));
  else if(!fixp(reg1)) zerror(39,reg1,@);
  else zerror(39,reg2,@);
}
/*@*/
Logxor2()
{
  if(fixp(reg1) && fixp(reg2)) reg1 = zinteger(intval(reg1) ^ intval(reg2));
  else if(!fixp(reg1)) zerror(39,reg1,@);
  else zerror(39,reg2,@);
}
/*@*/
Lposn()
{
  reg1 = zinteger((long) currlin);
}
/*@*/
Map()
{
  kalloc(2); kset(1,reg2);
  while(!null(reg1))
    { kset0(reg1);
      dummypair.Xcar = reg1;
      reg1 = local(1); reg2 = Sexp(&dummypair); Apply();
      reg1 = cdr(local0);
    }
  kpop(2);
}
/*@*/
Mapc()
{
  kalloc(2); kset(1,reg2);
  while(!null(reg1))
    { kset0(reg1);
      dummypair.Xcar = car(reg1);
      reg1 = local(1); reg2 = Sexp(&dummypair); Apply();
      reg1 = cdr(local0);
    }
  kpop(2);
}
/*@*/
Mapc2()
{
 if(null(reg1)) return;
 kalloc(3); kset0(cdr(reg1));
 reg1 = car(reg1);
 Mapcar(); local(1) = local(2) = zcons1(reg1);
 while(!null(local0))
   { reg1 = car(local0);
     Mapcar();
     local(2) = cdr(local(2)) = zcons1(reg1);
     kset0(cdr(local0));
   }
 reg1 = local(1);
 kpop(3);
}
/*@*/
Mapcan()
{
  if (null(reg1)) return;
  if(atom(reg1)) goto err;
  kalloc(5);
  dummypair.Xcar = car(reg1);
  kset(0,cdr(reg1));
  local(1) = reg1 = reg2;
  reg2 = Sexp(&dummypair);
  Apply();
  kset(2,reg1);
  reg1 = local(0);
  local(3) = local(4) = zcons1(NIL);
  goto lab3;
lab1 :
  kset0(reg1);
  dummypair.Xcar = car(reg1);
  reg2 = Sexp(&dummypair);
  kload(reg1,1);
  Apply();
  if (null(reg1)) goto lab2;
  reg2 = reg1;
  kload(reg1,4);
  kset(4,reg2);
  Nconc();
lab2 :
  reg1 = cdr(local0);
lab3 :
  if (pairp(reg1)) goto lab1;
  if(!null(reg1)) goto err;
  reg1 = cdr(local(3));
  reg2 = reg1;
  kload(reg1,2);
  Nconc();
lab4 :
  kpop(5);
  return;
err:  zerror(0, reg1, Sexp(&npair), @);
}

/*@*/
Mapcar()       /* reg2 preserved */
{
  if(null(reg1)) zreturn(NIL)
  kalloc(2); kset0(reg1); kset(1,reg2);
  dummypair.Xcar = car(reg1);
  reg1 = reg2; reg2 = Sexp(&dummypair); Apply(); reg2 = local(1);
  kset(1,reg1); reg1 = cdr(local0); Mapcar();
  zcons3c(local(1));
  kpop(2);
}
/*@*/
Mapcon()
{
  if (null(reg1)) return;
  if(atom(reg1)) goto err;
  kalloc(5);
  dummypair.Xcar = reg1;
  kset(0,cdr(reg1));
  local(1) = reg1 = reg2;
  reg2 = Sexp(&dummypair);
  Apply();
  kset(2,reg1);
  reg1 = local(0);
  local(3) = local(4) = zcons1(NIL);
  goto lab3;
lab1 :
  kset0(reg1);
  dummypair.Xcar = reg1;
  reg2 = Sexp(&dummypair);
  kload(reg1,1);
  Apply();
  if (null(reg1)) goto lab2;
  reg2 = reg1;
  kload(reg1,4);
  kset(4,reg2);
  Nconc();
lab2 :
  reg1 = cdr(local0);
lab3 :
  if (pairp(reg1)) goto lab1;
  if(!null(reg1)) goto err;
  reg1 = cdr(local(3));
  reg2 = reg1;
  kload(reg1,2);
  Nconc();
lab4 :
  kpop(5);
  return;
err:  zerror(0, reg1, Sexp(&npair), @);
}

/*@*/
Maplist()
{
  if(null(reg1)) zreturn(NIL)
  kalloc(3); kset0(reg1); kset(1,reg2);
  dummypair.Xcar = reg1;
  reg1 = reg2; reg2 = Sexp(&dummypair); Apply(); kset(2,reg1);
  reg2 = local(1); reg1 = cdr(local0); Maplist();
  zcons3c(local(2));
  kpop(3);
}
/*@*/
Max()
{
  kalloc(2); kset0(cdr(reg1)); kset(1,reg2); reg1 = car(reg1);
  Eval(); reg2 = reg1;
  while(!null(local0))
    { reg1 = car(local0); Eval();
      Max2(); reg2 = reg1;
      kset0( cdr(local0));
    }
  reg2 = local(1);
  kpop(2);
}
/*@*/
Max2()
{ PSEXP x;        /* changed after big # */
  x = reg1;
  Greaterp();
  reg1 = reg1 == T ? x : reg2;
}
/*@*/
Member()
{
  PSEXP u, v;

  u = reg1; v = reg2;
  while(1)
    { if(null(v)) zreturn(NIL)
      reg2 = car(v);  Equal();
      if(!null(reg1)) zreturn(v)
      reg1 = u;
      v = cdr(v);
    }
}
/*@*/
Memq()
{
  while(1)
    { if(null(reg2)) zreturn(NIL)
      if(reg1 == car(reg2)) zreturn(reg2)
      reg2 = cdr(reg2);
    }
}
/*@*/
Min()
{
  kalloc(2); kset0(cdr(reg1)); kset(1,reg2); reg1 = car(reg1);
  Eval(); reg2 = reg1;
  while(!null(local0))
    { reg1 = car(local0); Eval();
      Min2(); reg2 = reg1;
      kset0( cdr(local0));
    }
  reg2 = local(1);
  kpop(2);
}
/*@*/
Min2()
{ PSEXP x;        /* changed after big # */
  x = reg1;
  Greaterp();
  reg1 = reg1 == T ? reg2 : x;
}
/*@*/
Minus()
{                                /* changed after big # */
 switch(type(reg1)){
    case Tinteger:   reg1 = zinteger(-intval(reg1)); return;
    case Tfloating:  reg1 = zfloating(-floval(reg1)); return;
    case Tbig:       reg1 = zbig(-bigval(reg1),bigcdr(reg1)); return;
    default:         zerror(12, reg1, @); }
}
/*@*/
Minusp()
{                          /* changed after big # */
  switch(type(reg1)){
    case Tinteger:   if(intval(reg1) < 0) goto jt; break;
    case Tfloating:  if(floval(reg1) < 0) goto jt; break;
    case Tbig:       if(bigval(reg1) < 0) goto jt; break;
    default:         zerror(12, reg1, @); }
  reg1 = NIL;  return;
jt:
  reg1 = T;
}
/*@*/
Mkvect()
{ int n,m;
  PSEXP v;
  PPSEXP x;

  m = intval(reg1);
  if(m < 0 || m > 16000) zerror(10, reg1);    /*system dependent TY*/
  v = Sexp(zalloc(Tvector));
  upbv(v) = m;
  x = (PPSEXP) myalloc((unsigned) (m+1)*sizeof(PSEXP));  /*ver 5.1 TY*/
  if(x == NULL) zerror(10, reg1);
  vectelts(v) = x;
  usedvec(v) = 1;   /*ver 5.2 TY*/
  for(n=0; n<=m; n++) *x++ = NIL;
  reg1 = v;
}
/*@*/
Nconc()
{
  PSEXP w;

  if(null(reg1)) zreturn(reg2)
  if(atom(reg1)) zerror(0, reg1, Sexp(&npair), @);
  w = reg1;
  while(!null(cdr(w)))
    if(atom(w = cdr(w))) zerror(0, w, Sexp(&npair), @);   /* 6.0 TY */
  cdr(w) = reg2;
}
/*@*/
Ncons()
{
  reg1 = zcons1(reg1);
}
/*@*/
Not()
{
  Null();
}
/*@*/
Null()
{
  booleanp(null(reg1));
}
/*@*/
Numberp()
{ char i;
  booleanp((i = type(reg1)) == Tinteger || i == Tbig || i == Tfloating);
}
/*@*/
Oblist()
{ PSEXP u;
  unsigned n;

  reg1 = NIL;
  for(n=0; n<128; n++)
    { u = Sexp(hashtab[n]);
      while(u != NULL)
        { if (inoblistp(u)) zcons3c(u);
          u =Sexp(hashlink(u)); }
     }
}
/*@*/
Open1()
{
  FILE *fp;
  char *fl;
  char b[50],rw[3];
  char *x,*y;
  extern char *getenv();

  if(stringp(reg1)) fl = strbody(reg1);
  else if(idp(reg1)) fl = pname(reg1);
  else zerror(27, reg1, @);
  if(reg2 == Sexp(&inpt)) strcpy(rw, "r");
  else if(reg2 == Sexp(&outpt)) strcpy(rw, "w");
  else zerror(22, reg2);
  fp = fopen(fl,rw);
  if(fp == NULL) x = getenv("PATH");
  while(fp == NULL &&  x != NULL)
    { y = b;
      while((*x != ';') && (*x != 0)) *y++ = *x++;
      if(*x == ';') x++;
      if(*x == 0) x = NULL;
      *y = 0;
      y--;
      if(*y != '\\') strcat(b, "\\");
      strcat(b,fl);
      fp = fopen(b,rw);
    }
  if(fp == NULL) zerror(23, reg1);
  reg1 = zinteger((long) fp);
}

/*@*/
Onep()
{ switch(type(reg1)){
    case Tinteger:   booleanp(reg1 == Sexp(&ONE)); return;
    case Tfloating:  booleanp(floval(reg1) == 1.0); return;
    default :        booleanp(0); return; }
}
/*@*/
Or()
{
  if(null(reg1)) zreturn(NIL)
  kalloc(1);
  while(1)
    { if (null(reg1)) {kpop(1); return; }  /*GU*/
      kset0(reg1);
      reg1 = car(reg1); Eval();
      if(!null(reg1)) { kpop(1); return; }
      reg1 = cdr(local0);
    }
}

/*@*/
Orderp()
{
  if(type(reg1) < type(reg2)) goto jtrue;
  else if(type(reg1) != type(reg2)) goto jnil;
  switch(type(reg1))
   {
    case Tid :
      if(strcmp(pname(reg1), pname(reg2)) > 0) goto jnil;
      break;
    case Tstring :
      if(strcmp(strbody(reg1), strbody(reg2)) > 0) goto jnil;
      break;
    case Tinteger  :
    case Tfloating :
    case Tbig      : Lessp(); return;
    default :
      zerror(36,reg1,@);
   }
jtrue :
  reg1 = T;
  return;
jnil :
  reg1 = NIL;
}
/*@*/
Pagelength()
{
  PSEXP v;

  v = zinteger((long) pageln);
  if(null(reg1)) zreturn(v)
  if(fixp(reg1) && intval(reg1) >= 0)
    pageln = intval(reg1);
  else
    zerror(30, reg1);
  reg1 = v;
}
/*@*/
Pair()
{
  if(null(reg1) && null(reg2)) return;
  if(atom(reg1)) zerror(0, reg1, Sexp(&npair), @);
  if(atom(reg2)) zerror(0, reg2, Sexp(&npair), @);
  kalloc(3);
  local0 = zcons(car(reg1), car(reg2));
  local(1) = local(2) = zcons1(local0);
  while(1)
    { reg1 = cdr(reg1); reg2 = cdr(reg2);
      if(atom(reg1))
        if(atom(reg2)) zkreturn(local(1),3)
          else zerror(14);
      if(atom(reg2)) zerror(14);
      local0 = zcons(car(reg1), car(reg2));
      local(2) = cdr(local(2)) = zcons1(local0);
    }
}
/*@*/
Pairp()
{
  booleanp(pairp(reg1));
}
void nextnum()   /* utility used by Plus */
{ reg1 = car(local0); Eval();
  local0 = cdr(local0);
}
/*@*/
Plus()
{
  long int n;
  double a;

  if(!pairp(reg1)) goto err;
  n = 0L;
  kalloc(2);
  kset0(reg1);
               /* let us try fastest way first, assuming small integers */
  while(!null(local0))
    { nextnum();
      switch(type(reg1)){
        case Tinteger:
          n += intval(reg1);
          if(labs(n)>BM1) { znormalize(n); goto bign; }
          break;
        case Tfloating:
          a = floval(reg1) + n;
          goto flo;  /* from now on floating point addition only. */
        case Tbig: if(n)
                     { kset(1,reg2);
                       reg2 = zinteger(n);
                       (void)addsub(1);
                       goto bign2; }
                   else goto bign;
        default:   goto err;  }  /* end of switch */
    }
  reg1 = zinteger(n);
  kpop(2);
  return;
err:
  zerror(12, reg1, @);
                /* from now on integers are big */
bign:
  kset(1,reg2);
bign2:
  reg2 = reg1;          /* from now on reg2 contains latest big result */
  while(!null(local0))
    { nextnum();
      switch(type(reg1)){
        case Tinteger:
        case Tbig:   (void)addsub(1); reg2 = reg1; break;
        case Tfloating:
          a = floval(reg1) + zbig2float(reg2);
          kload(reg2,1);
          goto flo;    /* from now on floating point addition only. */
        default:   goto err;  }  /* end of switch */
    }
   kload(reg2,1); kpop(2); return;  /* reg1 is containing result already */
flo:
  while(!null(local0))
    { nextnum();
      switch(type(reg1)){
        case Tinteger:   a += intval(reg1); break;
        case Tbig:       a += zbig2float(reg1); break;
        case Tfloating:  a += floval(reg1); break;
        default:   goto err;  }  /* end of switch */
    }
  reg1 = zfloating(a);
  kpop(2);
}
/*@*/
Plus2()
{ switch(addsub(1)) {
    case 0: return;
    case 1: zerror(12, reg1, @);
    case 2: zerror(12, reg2, @); }
}
/*@*/
Posn()
{
  reg1 = zinteger((long) currcol);
}
/*@*/
Prin1()
{
  PSEXP x, y;
  int i,j,k;

  switch(type(reg1)) {                /*GU*/
    case Tpair : y = reg1;
                 x = reg1;
                 zedit("(");
                 while(1)
                   { reg1 = car(y); Prin1();
                     y = cdr(y);
                     if(!pairp(y)) break;
                     zedit(" "); }
                 if(!null(y))
                   { zedit(" . ");
                     reg1 = y; Prin1(); }
                 zedit(")");
                 reg1 = x;
                 break;
    case Tbig :  if((i = (int)bigval(reg1)) < 0) zedit("-");
                   i = abs(i);
                 reg1 = bigcdr(reg1);
                 sprintf(tx, "%ld", bigval(reg1));
                 zedit(tx);
                 for(j=1;  j<i; j++)
                    { reg1 = bigcdr(reg1);
                      sprintf(tx, "%ld", bigval(reg1));
                      k = strlen(tx);
                      while(k++ < DZ) zedit("0");
                      zedit(tx); }
                 return;
    case Tvector : zedit("[");
                   x = reg1;
                   { unsigned n;
                     for(n=0; n<=upbv(x); n++)
                       { if(n) zedit(",");
                         reg1 = vectelt(x,n); Prin1();
                       }
                   }
                   zedit("]");
                   reg1 = x;
                   break;
    default : zescape(reg1);
              zedit(tx);
              break;
  }
}
/*@*/
Prin2()
{
  PSEXP x, y;

  switch(type(reg1)) {
    case Tpair : y = reg1;
                 x = reg1;
                 zedit("(");
                 while(1)
                   { reg1 = car(y); Prin2();
                     y = cdr(y);
                     if(!pairp(y)) break;
                     zedit(" "); }
                 if(!null(y))
                   { zedit(" . ");
                     reg1 = y; Prin2(); }
                 zedit(")");
                 reg1 = x;
                 break;
    case Tid : zedit(pname(reg1));
               break;
    case Tstring : zedit(strbody(reg1));
                   break;
    case Tbig     :
    case Tinteger : Prin1();
                    break;
    case Tfloating : sprintf(tx, "%ld", floval(reg1));
                     zedit(tx);
                     break;
    case Tvector : zedit("[");
                   x = reg1;
                   { unsigned n;
                     for(n=0; n<=upbv(x); n++)
                       { if(n) zedit(",");
                         reg1 = vectelt(x,n); Prin2();
                       }
                   }
                   zedit("]");
                   reg1 = x;
                   break;
    case Tfpointer : sprintf(tx, "#%lo#", (long) (Scode(reg1)->Xfnc));
                     zedit(tx);
                     break;
  }
}
/*@*/
Princ()
{
  if(!idp(reg1)) zerror(28, reg1, @);
  if(reg1 == Sexp(&eol))
    { putc('\n',outputf);   /*quick code   TY*/
      cnewline;
      currcol = 0;
      return;
    }
  if(pname(reg1)[1]) zerror(28, reg1, @);
  zedit(pname(reg1));
}
/*@*/
Print()
{ PSEXP x;

  x = reg1;
  Prin1();
  Terpri();
  reg1 = x;
}
/*@*/
Prog()
{
  PSEXP x;
  kalloc(2);
  kset(1,reg1);
  x = car(reg1);
  while(x != NIL)
    { zalist(car(x), value(car(x)));
      value(car(x)) = NIL;
      x = cdr(x);
    }
  kset0(cdr(reg1));
  while(1)
    { while(!null(local0))
       { if(!idp(car(local0)))
           { reg1 = car(local0); Eval();
             if(errmsgp(reg1)) break;
           }
         kset0( cdr(local0));
       }
     if(null(local0))
       { x = car(local(1));
         while(x != NIL)
           { zpopalist();
             x = cdr(x);
           }
         kpop(2);
         zreturn(NIL)
       }
     if(errorno(reg1) == 1)
       { x = car(local(1));
         while(x != NIL)
           { zpopalist();
             x = cdr(x);
           }
         kpop(2);
         zreturn(errormsg(reg1))
       }
     kset0( cdr(local(1)));
     while(1)
       { if(null(local0)) zerror(6, errormsg(reg1));
         if(car(local0) == errormsg(reg1)) break;
         kset0( cdr(local0));
       }
   }
}
/*@*/
Prog2()
{ reg1 = reg2; }
/*@*/
Progn()
{
  if(null(reg1)) return;
  if(atom(reg1)) zerror(0, reg1, Sexp(&npair), @);
  kalloc(1);
  kset0(reg1);
  while(!null(local0))
    { reg1 = car(local0); Eval();
      kset0(cdr(local0));
    }
  kpop(1);
}
/*@*/
Put()         /* preserves reg2,reg3 */
{
  PSEXP z;

  if(!idp(reg1)) zerror(0, reg1, Sexp(&nid), @);
  if(!idp(reg2)) zerror(0, reg2, Sexp(&nid), @);
  z = proplist(reg1);
  while(!null(z))
    { if(pairp(car(z)) && caar(z) == reg2)
        { cdar(z) = reg3; goto end; }
      z = cdr(z);
    }
  kalloc(1);
  local0 = zcons(reg2,reg3);
  proplist(reg1) = zcons(local0,proplist(reg1));
  kpop(1);
end:
  reg1 = reg3;
}
/*@*/
Putd()
{
#if BITF
  if(!idp(reg1) || globalp(reg1) || fluidp(reg1)) zerror(2, reg1);
#else
  if(!idp(reg1) || (attribute(reg1) & 0x18)) zerror(2, reg1);
#endif
  if(reg2 != Sexp(&expr) && reg2 != Sexp(&fexpr) && reg2 != Sexp(&macro)
/*   && reg2 != Sexp(&lexpr) && reg2 != Sexp(&nexpr)  */
     && reg2 != Sexp(&subr) && reg2 != Sexp(&fsubr)) { Put(); return; }
  if(functionp(reg1)) printf("\n*** %s redefined\n",pname(reg1));
#if BITF
  functionp(reg1) = 1;
#else
  attribute(reg1) |= 0x04;
#endif
  value(reg1) = zcons(reg2,reg3);
}
/*@*/
Putv()
{
  if(!vectorp(reg1)) zerror(0, reg1, Sexp(&nvector), @);
  if(!fixp(reg2)) zerror(0, reg2, Sexp(&ninteger), @);
  if(intval(reg2) < 0 || intval(reg2) > upbv(reg1)) zerror(9, reg2);
  vectelt(reg1, intval(reg2)) = reg3;
  reg1 = reg3;
}
/*@*/
Quit()
{
  exit(0);
}
/*@*/
Quote()
{
  Car();
}
/*@*/
Quotient()
{ double d;
  switch(type(reg1)){
    case Tinteger:
       switch(type(reg2)){
         case Tinteger:
         case Tbig:      goto jint;
         case Tfloating: d = intval(reg1) / floval(reg2); goto dbl;
         default:        goto err; }
    case Tfloating:
       switch(type(reg2)){
         case Tinteger: d = floval(reg1) / intval(reg2); goto dbl;
         case Tbig:     d = floval(reg1) / zbig2float(reg2); goto dbl;
         case Tfloating:d = floval(reg1) / floval(reg2); goto dbl;
         default:       goto err; }
    case Tbig:
       switch(type(reg2)){
         case Tinteger:
         case Tbig:      goto jint;
         case Tfloating: d = zbig2float(reg1) / floval(reg2); goto dbl;
         default:        goto err; }
    default: zerror(12,reg1,@);  }
err: zerror(12,reg2,@);
jint: zdivision(1); return;
dbl : reg1 = zfloating(d);
}
/*@*/
Rds()
{
  PSEXP v;
  /* curpos = ""; */                           /* 7.0 removes it */
  store_input_buf_save_entry();
  if(inputf != stdin) v = zinteger((long) inputf);
    else v = NIL;
  if(null(reg1)) { inputf = stdin;
                   restore_input_buf_save_entry(); }      /* 7.0 */
  else if(fixp(reg1)) { inputf = (FILE *) intval(reg1);
                        restore_input_buf_save_entry(); } /* 7.0 */
  else zerror(24, reg1);
  reg1 = v;
}
/*@*/
Read1()
{
  Token();
  if(delimflag && reg1 == ch('\'') )
    { Read1(); Ncons();
      zcons3c(quote); return; }
  if(!delimflag || reg1 != ch('(') ) return;
  Read1();
  if(delimflag && reg1 == ch(')')) zreturn(NIL)
  kalloc(2);
  local(1) = local0 = zcons1(reg1);
  while(1)
     { Read1();
       if(delimflag && reg1 == ch(')') )
         { delimflag = 0; break; }
       if(delimflag && reg1 == ch('.') )
         { Read1();
           if(delimflag && reg1 == ch(')') )
             { delimflag = 0; break; }
           else
             { cdr(local0) = reg1;
               while(1)
                { Read1();
                  if(delimflag && reg1 == ch(')') ) break; }
               delimflag = 0;
               goto end; }
         }
       local0= cdr(local0) = zcons1(reg1);
     }
end:
  zkreturn(local(1),2)
}
/*@*/
Readch()
{
  char c;

  switch(c = yych()) {
    case EOF : zreturn(Sexp(&eof))
    case '\n' : zreturn(Sexp(&eol))
    default : zreturn(ch(c))
   }
}
/*@*/
Remainder()
{ char x;
  x = type(reg1);
  if(!((x == Tinteger) || (x == Tbig))) zerror(0, reg1, Sexp(&ninteger), @);
  x = type(reg2);
  if(!((x == Tinteger) || (x == Tbig))) zerror(0, reg2, Sexp(&ninteger), @);
  zdivision(0);
}
/*@*/
Remd()
{
  PSEXP v;
  if(!idp(reg1) || !functionp(reg1)) zreturn(NIL)
  v = value(reg1);
  value(reg1) = NULL;
#if BITF
  functionp(reg1) = 0;
#else
  attribute(reg1) &= 0xfb;
#endif
  reg1 = v;
}
/*@*/
Remflag()              /*21:26:09  11/10/1986*/
{
  PSEXP z;

  if(!idp(reg2)) zerror(0, reg2, Sexp(&nid), @);
  while(!null(reg1))
    {if(!pairp(reg1)) zerror(0, reg1, Sexp(&npair), @);
     if(!idp(car(reg1))) zerror(0, car(reg1), Sexp(&nid), @);
     if(null(z = proplist(car(reg1))))
       { reg1 = cdr(reg1);
         continue;
       }
     if(car(z) == reg2)
       { proplist(car(reg1)) = cdr(z);
         continue;
       }
     while(!null(cdr(z)))
       { if(cadr(z) == reg2)
           { cdr(z) = cddr(z);
             break;
           }
         z = cdr(z);
       }
     reg1 = cdr(reg1);
   }
 reg1 = NIL;
}
/*@*/
Remob()
{
   if(!idp(reg1)) zerror(0, reg1, Sexp(&nid), @);
#if BITF
   inoblistp(reg1) = 0;
#else
   attribute(reg1) &= 0xfd;
#endif
   /* Changed ver 3.3 from now on every atom is in a hash bucket
      being in the oblist is due to set flag:Xisinoblist */
}
/*@*/
Remprop()
{
  PSEXP z;

  if(!idp(reg1) || !idp(reg2) || null(z = proplist(reg1))) goto jnil;
  if(pairp(car(z)) && caar(z) == reg2)
    { proplist(reg1) = cdr(z);
      zreturn(cdar(z))  }
  while(!null(cdr(z)))
    { if(pairp(cadr(z)) && caadr(z) == reg2)
        { reg1 = cdadr(z);
          cdr(z) = cddr(z);
          return; }
      z = cdr(z);
    }
jnil:
  zreturn(NIL)
}
/*@*/
Return()
{
  forprog.Xerrorno = 1;
  forprog.Xerrormsg = reg1;
  reg1 = Sexp(&forprog);
}
/*@*/
Reverse()
{ if(null(reg1)) return;
  if(atom(reg1)) zerror(0, reg1, Sexp(&npair), @);
  kalloc(1); kset0(reg1);
  reg1 = NIL;
  while(pairp(local0))
    { zcons3c(car(local0));
      kset0(cdr(local0));
    }
  kpop(1);
}
/*@*/
Reversip()
{
  PSEXP u,v;
  if(null(reg1)) return;
  if(atom(reg1)) zerror(0, reg1, Sexp(&npair), @);
  v = NIL;
  while(pairp(u = reg1))
    { reg1 = cdr(reg1);
      cdr(u) = v;
      v = u;
    }
  reg1 = v;
}
/*@*/
Rplaca()
{
  if(pairp(reg1)) car(reg1) = reg2;
  else
    zerror(0, reg1, Sexp(&npair), @);
}
/*@*/
Rplacd()
{
  if(pairp(reg1)) cdr(reg1) = reg2;
  else
    zerror(0, reg1, Sexp(&npair), @);
}
/*@*/
Sassoc()                       /*modified completely TY*/
{
  if(atom(reg2)) zerror(26,reg2);
  kalloc(2);
  kset0( reg1);
lab0 :
  if (!null(reg2)) goto lab1;
  reg1 = reg3;
  Apply();
  goto lab3;
lab1 :
  if(atom(car(reg2))) zerror(26,reg2);
  kset(1, reg2);
  reg2 = car(car(reg2));
  Equal();
  if (null(reg1)) goto lab2;
  reg1 = car(local(1));
  goto lab3;
lab2 :
  reg2 = cdr(local(1));
  reg1 = local0;
  goto lab0;
lab3 :
  kpop(2);
  return; }

/*@*/
Select()
{
  if(atom(reg1) || atom(cdr(reg1))) zerror(31);
  kalloc(2);
  kset0(reg1); kset(1,reg2);
  reg1 = car(reg1); Eval(); reg2 = reg1;
  kset0( cdr(local0));
  while(!null(cdr(local0)))
    { if(atom(car(local0))) zerror(31);
      reg1 = caar(local0); Eval(); Equal();
      if(!null(reg1))
        { reg1 = cadar(local0); Eval();
          reg2 = local(1); kpop(2); return; }
      kset0( cdr(local0));
      if(atom(local0)) zerror(31);
    }
  reg1 = car(local0); kpop(2);
  Eval();
}
/*@*/
Set()
{
#if TRACEABLE
  if(ztracearg($,2)) goto exit;
#endif
  if(!idp(reg1)) zerror(0, reg1, Sexp(&nid), @);
  if(reg1 == NIL || reg1 == T) zerror(5);
  if(functionp(reg1)) zerror(3,reg1);
  reg1 = value(reg1) = reg2;
#if TRACEABLE
exit: ztraceval($);
#endif
}
/*@*/
Setpchar()
{ PSEXP u;
  u = value(&prompt);
  value(&prompt) = reg1;
  reg1 = u;
}
/*@*/
Setq()
{ PSEXP z;
#if TRACEABLE
  if(ztracearg($,1)) goto exit;
#endif
  if(!idp(z = car(reg1))) zerror(0, z, Sexp(&nid), @);
  if(z == NIL || z == T) zerror(5);
  if(functionp(z)) zerror(3,z);
  reg1 = cdr(reg1);
  if(value(z) == NULL) value(z) = z; /* just for GC purposes. protection of id. TY*/
  Car();
  kalloc(1);                  /* for compactified GC purposes */
  local0 = z;
  Eval();
  value(local0) = reg1;
  kpop(1);
#if TRACEABLE
exit: ztraceval($);
#endif
}
/*@*/
Signoff()
{
  zysignonoff(0);
  reg1 = NIL;
}
/*@*/
Signon()
{
  zysignonoff(1);
  reg1 = NIL;
}
/*@*/
Standardfzlisp()
{ initregs();
  reg1 = Sexp(&begin);  /*begin SHOULD BE DEFINED INTERPRETIVELY ALWAYS*/
  curtrap = trap;
  curtrap->msgprint = curtrap->backtrace = 1;
  curtrap->curalist = alisttop;
  curtrap->stacksave = zstackptr;
  setjmp(curtrap->errsave);
  if(reg1 == Sexp(&begin))
    { Getd();
      if(pairp(reg1) && car(reg1) == Sexp(&expr))
        { reg1 = cdr(reg1); reg2 = NIL; Apply(); }
    }
#if IBMPC
/* arbirary precision integer arithmetic after version 5.0 TY */
  printf("ษออออออออออออออออออออออออออออออออออออออออออออออออออป\n");
  printf("บ    S  T  D     L  I  S  P      [7.0] December 99 บ\n");
  printf("ศออออออออออออออออออออออออออออออออออออออออออออออออออผ\n");
#else
  printf("    S  T  D     L  I  S  P      [7.0]  December 99\n");
#endif
  curtrap = trap;
  setjmp(curtrap->errsave);
  yytext[0] = 0;
  curpos = "";
  inputf = stdin;
  outputf = stdout;
  while(1)
    { if(inputf == stdin)
        { printf("\neval:\n");
          cnewline;
        }
      initregs();     /*Help GC  TY*/
      Read1();
      if(reg1 == Sexp(&eof))
        { curpos = "";
          if(inputf != stdin) fclose(inputf);
          inputf = stdin; }   /* close the file if hit eof */
      else Eval();
      if(currcol)
        { putc('\n', outputf);
          cnewline;
          currcol = 0;
        }
      if(outputf == stdout)
        { printf("\nvalue:\n");
          cnewline;
        }
      Print();
    }
}
/*@*/
Stringp()
{
  booleanp(stringp(reg1));
}
/*@*/
Sub1()
{ if(addsub1(-1)) zerror(12, reg1, @);
}

/*@*/
Sublis()     /* compiler generated code GU */
{            /* human modified!!  TY*/
  if (null(reg1)) zreturn(reg2)
  kalloc(3);
  kset0( reg1);
  kset(1, reg2);
  reg2 = reg1;
  reg1 = local(1);
  Assoc();
  if(null(reg1)) goto lab2;
  reg1 = cdr(reg1);
  goto lab4;
lab2 :
  if(pairp(local(1))) goto lab3;
  reg1 = local(1);
  goto lab4;
lab3 :
  reg2 = car(local(1));
  reg1 = local0;
  Sublis();
  kset(2, reg1);
  reg2 = cdr(local(1));
  reg1 = local0;
  Sublis();
  zcons3c(local(2));
lab4 :
  kpop(3);
  return; }

/*@*/
Subst()
{
  if(null(reg3)) zreturn(NIL)
  kalloc(3); kset0(reg1); kset(1,reg2);
  reg1 = reg3; Equal();
  if(!null(reg1)) zkreturn(local0,3)
  if(atom(reg3)) zkreturn(reg3,3)
  kset(2,reg3);
  reg1 = local0;
  reg2 = local(1);
  reg3 = cdr(local(2));
  Subst();
  reg2 = local(1);
  reg3 = car(local(2));  kset(1,reg1);
  reg1 = local0;
  Subst();
  zcons2c(local(1));
  kpop(3);
}

/*@*/
System1()
{
  char *sc;

  if(stringp(reg1)) sc = strbody(reg1);
  else if(idp(reg1)) sc = pname(reg1);
  else zerror(27, reg1, @);
  reg1 = zinteger((long) system(sc));
}
/*@*/
Terpri()
{
  putc('\n',outputf);
  cnewline;
  currcol = 0;
  reg1 = NIL;
}
/*@*/
Time()
{ long time();        /* declaration may be removed if necessary TY*/
  znormalize(time());
}
/*@*/
Times()
{
  longdbl n,m;
  double a;

  if(!pairp(reg1)) goto err;
  n = m = 1.0;
  kalloc(2);
  kset0(reg1);
               /* let us try fastest way first, assuming small integers */
  while(!null(local0))
    { nextnum();
      switch(type(reg1)){
        case Tinteger:
          n *= intval(reg1);
          if(fabs(n) >= BFT) goto bign;
          m = n;
          break;
        case Tfloating:
          a = floval(reg1) * m;
          goto flo;  /* from now on floating point addition only. */
        case Tbig: goto bign;
        default:   goto err;  }  /* end of switch */
    }
  zfnormalize(m);
  kpop(2); return;
err:
  zerror(12, reg1, @);
                /* from now on integers are big */
bign:
  kset(1,reg2);
  if(m != 1.0)
    { kset(1,reg2);
      reg2 = reg1;
      zfnormalize(m);
      zmultiply(); }
  reg2 = reg1;          /* from now on reg2 contains latest big result */
  while(!null(local0))
    { nextnum();
      switch(type(reg1)){
        case Tinteger:
        case Tbig:   zmultiply(); reg2 = reg1; break;
        case Tfloating:
          a = floval(reg1) * zbig2float(reg2);
          kload(reg2,1);
          goto flo;    /* from now on floating point addition only. */
        default:   goto err;  }  /* end of switch */
    }
   kload(reg2,1); kpop(2); return;  /* reg1 is containing result already */
flo:
  while(!null(local0))
    { nextnum();
      switch(type(reg1)){
        case Tinteger:   a *= intval(reg1); break;
        case Tbig:       a *= zbig2float(reg1); break;
        case Tfloating:  a *= floval(reg1); break;
        default:   goto err;  }  /* end of switch */
    }
  reg1 = zfloating(a);
  kpop(2);
}
/*@*/
Times2()
{ double d;
  switch(type(reg1)){
    case Tinteger:
       switch(type(reg2)){
         case Tinteger:
         case Tbig:      zmultiply(); return;
         case Tfloating: d = intval(reg1)*floval(reg2);  goto dbl;
         default:        goto df; }
    case Tfloating:
       switch(type(reg2)){
         case Tinteger: d = floval(reg1)*intval(reg2); goto dbl;
         case Tbig:     d = floval(reg1)*zbig2float(reg2); goto dbl;
         case Tfloating:d = floval(reg1)*floval(reg2); goto dbl;
         default:       goto df; }
    case Tbig:
       switch(type(reg2)){
         case Tinteger:
         case Tbig:     zmultiply(); return;
         case Tfloating:d = zbig2float(reg1)*floval(reg2); goto dbl;
         default:       goto df; }
    default:  zerror(12, reg1, @); }
df:  zerror(12, reg2, @);
dbl: reg1 = zfloating(d);
}
/*@*/
Token()
{
  double atof();
  char c, *cp,*cx;
  long int i;

  switch(yylex()) {
    case Tid : if(delimflag)
                 zreturn(ch(yytext[0]))
               else
                 { cp = yytext;
                   cx = tx;
                   while((c = *cp++) != 0)
                     { if(c == '!')  *cx++ = *cp++;
                       else *cx++ = c;
                     }
                   *cx = 0;
                   zintern(1); return;  /*Changed ver 3.3 */
                 }
    case Tinteger : cp = yytext;
                    if(*cp == '-' || *cp == '+') cp++;
                    i =0l;
                    while(*cp)
                      i = 10*i + *cp++ - '0';
                    if(*yytext == '-') i = -i;
                    zreturn(zinteger(i))
    case Tbig     : zbigint(); return;
    case Tfloating: zreturn(zfloating(atof(yytext)))
    case Tstring : cp = yytext + 1;
                   cx = tx;
                   while(1)
                     { c = *cp++;
                       if(c == '"')
                         if(*cp)
                           { *cx++ = *cp++;
                             continue; }
                          else break;
                        *cx++ = c;
                     }
                   *cx = 0;
                   cp = zsalloc(strlen(tx));
                   reg1 = zalloc(Tstring);
                   strelement(reg1) = Sstrelement(cp);/*ver 3.3*/
#if IBMPC
                   father(reg1) = Sstr(reg1); */  /*New ver. 3.3*/
#else
                   setfather(reg1);
#endif
                   strcpy(strbody(reg1), tx); /* length was stored by zsalloc */
                   return;
    case EOF :     zreturn(Sexp(&eof))     /* new ver[5.0]   .GU */
  }
}
/*@*/
Traceable()
{ long l;
  if(null(reg1)) traceable = 0;
    else if(fixp(reg1))
           { Abs();
             l = intval(reg1);
             if(l > 0x7fff) traceable = 0x7fff;
               else traceable = (int)l; }
     else if(reg1 == T) traceable = 0x7fff;
}
/*@*/
Unfluid()
{
  while(!null(reg1))
    { if(!pairp(reg1)) zerror(0, reg1, Sexp(&npair), @);
      if(!idp(car(reg1))) zerror(0, car(reg1), Sexp(&nid), @);
      if(fluidp(car(reg1)))
#if BITF
      fluidp(car(reg1)) = 0;
#else
      attribute(car(reg1)) &= 0xf7;
#endif
      reg1 = cdr(reg1);
    }
}
/*@*/
Unlink1()
{
  char *fl;

  if(stringp(reg1)) fl = strbody(reg1);
  else if(idp(reg1)) fl = pname(reg1);
  else zerror(27, reg1, @);
  reg1 = zinteger((long) unlink(fl));
}
/*@*/
Upbv()
{
  reg1 = vectorp(reg1) ? zinteger((long) upbv(reg1)) : NIL;
}
/*@*/
Vectorp()
{
  booleanp(vectorp(reg1));
}
/*@*/
Wrs()
{
  PSEXP v;

  if(outputf != stdout) v = zinteger((long) outputf);
    else v = NIL;
  if(null(reg1)) outputf = stdout;
  else if(fixp(reg1)) outputf = (FILE *) intval(reg1);
  else zerror(25,reg1);
  reg1 = v;
}                                /*14:05:09  11/15/1986  TY*/
                     /*func. calls modified 19:24  5/2/1987  TY*/
/*@*/
Xcons()
{
  zcons3c(reg2);
}
/*@*/
Zerop()
{ switch(type(reg1)){
    case Tinteger  : booleanp(reg1 == Sexp(&ZERO)); return;
    case Tfloating : booleanp(floval(reg1) == 0.0); return;
    default :        booleanp(0); return; }
}
