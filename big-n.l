/* Arbitrary precision arithmetic support to LISP.
   division is not present yet. Trieste, Italy ICTP July 31,1989  13:11  TY */
/* division added at Ankara November 3, 1989 15:45 TY */

/* umultiply multiplies two long number; result returned in x and y.
   lower part in y and higher part in x. */

void umultiply(x,y)
long *x,*y;
{ longdbl z;        /* long double is a ten byte double defined in TURBOC */
  z = longd(*x) * (*y);
  *x = (long) (z/BASE);
  *y = (long) (z - longd(*x)*BASE);
}

long udivide(x,y,d)
long x,*y,d;       /* returns remainder */
{ longdbl z;
  if(x)
    { z = longd(x)*BASE + *y;
      *y = (long) (z/d);
      return((long) (z - longd(*y)*d));
    }
  else
    { x = *y;
      *y = x/d;
      return(x%d); }
}

void resize(p,n,l)
long **p;          /* resizes the arithmetic arrays */
int *n;
int l;
{ free(*p);
  *n = 3*l/2;
  *p = (long *)myalloc((*n)*sizeof(long));
}

PSEXP zbig(v,n)  /* prepares new cell for big number */
long v;
PSEXP n;
{ PSEXP z;
  z = Sexp(zalloc(Tbig));
  type(z) = Tbig;
  bigval(z) = v;
  bigcdr(z) = n;
  return(z);
}

long zintval(xp,dz)   /* ascii string of dz numbers pointed by xp --> bin number */
char *xp;
int dz;
{ long j = 0;
  int k;

  for(k=1; k<=dz; k++) j = 10*j + *xp++ - '0';
  return(j);
}

void zbigint()
{ int i,j,l=0;     /* parser of big numbers */
  int sg=0,s=1;

  reg1 = NIL;
  if(yytext[0] == '-'){ s = -1; sg = 1; }
   else if(yytext[0] == '+') sg = 1;
  j = digitcnt - DZ + sg;
  while(1)
    { reg1 = zbig(zintval(yytext+j ,DZ),reg1);
      l++;
      if((i = j - DZ)>=sg) j=i;
       else break;
    }
  if((j-=sg))
    { reg1 = zbig(zintval(yytext+sg ,j),reg1);
      l++; }
  reg1 = zbig((long) s*l,reg1);
}

double zbig2float(x)    /* big to float conversion */
PSEXP x;
{ int i,j,l,s=1;
  double r=0.0;

  if((l = (int)bigval(x))<0) s = -1;
  l = abs(l);
  j = min(l,4);
  for(i=0; i<j; i++)
     { x = bigcdr(x);
       r = r*BASE + bigval(x); }
  for(i=j; i<l; i++) r = r*BASE;
  return(s*r);
}

void znormalize(x)   /* checks the number sizes and put them in */
long x;                     /*  single cell if possible */
{ long x1;

  x1 = labs(x);
  if(x1<BASE) reg1 = zinteger(x);
   else
    { reg1 = zbig(x1%BASE,NIL);
      reg1 = zbig(x1/BASE,reg1);
      reg1 = zbig( x<0 ? -2L : 2L, reg1); }
}

void zfnormalize(x)   /* checks the number sizes and put them in */
longdbl x;              /*  single cell if possible */
{ int s;
  long a;
  longdbl dd;

  if(x<0.0) { dd = -x; s = -2; }
    else { dd = x; s = 2; }
  if(dd<BASE) reg1 = zinteger((long)x);
   else
      { a = (long) (dd/BASE) ;
        reg1 = zbig((long)(dd - longd(a)*BASE),NIL);
        reg1 = zbig(a,reg1);
        reg1 = zbig((long)s,reg1);
      }
}

void zmultiply()       /* arbitrary precision integer multiplication */
{ long a,b,rl,rh,s,*np;
  int l1,l2,l,i,j,u=-1;

  if(fixp(reg1))
    { a = intval(reg1);
      if(fixp(reg2))
        { zfnormalize(longd(a)*intval(reg2));    /* normal multiplication */
          return; }
      if(a == 1) {reg1 = reg2; return;}
      if(a == 0) return;
      *arit1 = labs(a);  /* placement of big numbers into arrays. access is quicker */
      l1 = 0;}
  else
    { a = bigval(reg1);
      l1 = labs(a)-1;
      if(arit1sz<=l1) resize(&arit1, &arit1sz,l1);
      np = arit1 + l1;
      for(i=0; i<=l1; i++)
        { reg1 = bigcdr(reg1);
          *np-- = bigval(reg1); }
    }
  if(fixp(reg2))
    { b = intval(reg2);
      if(b == 1) return;
      if(b == 0) { reg1 = reg2; return; }
      *arit2 = labs(b);
      l2 = 0;}
  else
    { b = bigval(reg2);
      l2 = labs(b)-1;
      if(arit2sz<=l2) resize(&arit2, &arit2sz,l2);
      np = arit2 + l2;
      for(i=0; i<=l2; i++)
        { reg2 = bigcdr(reg2);
          *np-- = bigval(reg2); }
    }
  if(a<0) s = -1; else s = 1;   /* carry sign separately */
  if(b<0) s *= -1;
  l = l1 + l2;
  reg1 = NIL;
  rl = rh = 0;
  for(i=0; i<=l; i++)          /* beginning point of multiplication */
    { if(i<=l2) u++;
      j = i>l1 ? i - l1 : 0;
      np = arit1 + i;
      do { a = *(np - j);   b = *(arit2 + j);
           umultiply(&a,&b);
           rl += b;
           rh += a + rl/BASE;
           rl %= BASE;
         } while (++j <= u);
      reg1 = zbig(rl,reg1);
      rl = rh%BASE;
      rh /= BASE;
    }
  if(rl){reg1 = zbig(rl,reg1); l++; }
  if(rh){reg1 = zbig(rh,reg1); l++; }
  reg1 = zbig((long)(s*(l+1)),reg1);
}   /* end of zmultiply */


void zexpt()
{ long a;
  int i;

  if(fixp(reg1))
    { if((a=intval(reg1)) == 1 || a == 0) return;
      if(a == -1)
        { if(fixp(reg2)){ if(!(intval(reg2) & 1)) goto r1; }
           else
             { i = abs((int)bigval(reg2));
               while(i-- >= 0) reg2 = bigcdr(reg2);
               if(!(bigval(reg2) & 1)) goto r1; }
           return;
        }
     }
   if(bigp(reg2))
     { if(bigval(reg2)<0) goto r0;
        else zerror(41); }
   if((a = intval(reg2))<0) goto r0;
    else if(a == 0) goto r1;
    else if(a == 1) return;
    else
      { kalloc(2);
        kset(1,reg1);  i = 1;
        while(a>0)
          { if(a&1)
              { if(i){ local0 = local(1); i = 0; }
                 else { reg1 = local0; reg2 = local(1);
                        zmultiply(); local0 = reg1; } }
            a = a >> 1;
            if(a>0){ reg1 = reg2 = local(1);
                     zmultiply(); kset(1, reg1); }
          }
      }
    reg1 = local0;
    kpop(2);
    return;
r1: reg1 = Sexp(&ONE);
    return;
r0: reg1 = Sexp(&ZERO);
}

#define addon(E)   z=bigcdr(z)=zbig(E,NIL)
#define laydown    if(e>0)addon(e);else if(z==Sexp(&dummybig))l--; \
                   else addon(e); \
                   if(k>0) do { addon(d); } while(--k != 0)
#define exchange   dd = reg1; reg1 = reg2; reg2 = dd; l1 = 2*l2 - l1

void zaddsub(s)
int s;
{ long h,e,d;
  int l1,l2,l,k=0,s1=1;
  PSEXP dd,z;

  if(fixp(reg1))
    { l1 = 1;
      h = intval(reg1);
      reg1 = zbig(labs(h),NIL); }
  else
    { h = bigval(reg1); l1 = labs(h);
      reg1 = bigcdr(reg1); }
  if(h<0){ s1 = -1; s *= -1; }
  if(fixp(reg2))
    { l2 = 1;
      e = intval(reg2);
      reg2 = zbig(labs(e),NIL); }
  else
    { e = bigval(reg2); l2 = labs(e);
      reg2 = bigcdr(reg2); }
  if(e<0) s *= -1;
  e = 0L;
  z = Sexp(&dummybig);
  l = l1>l2 ? l1 : l2;   /* max of l1,l2 */
  if(s>0)
    { if(l1<l2) { exchange;}
      do { if(l2<l1) { h = bigval(reg1); l2++; }
           else { h = bigval(reg1) + bigval(reg2);
                  reg2 = bigcdr(reg2); }
           if(h == BM1)  k++;
           else
             { if(h>BM1){ h -= BASE; e++; d = 0L; }
                else d = BM1;
               laydown;
               e = h; }
         } while((reg1 = bigcdr(reg1)) != NIL);
       d = BM1;
    }
  else    /* if s<0  */
    { if(l1 == l2)
        { while(bigval(reg1) == bigval(reg2))
            { if(--l == 0) {reg1 = Sexp(&ZERO); return; }
              reg1 = bigcdr(reg1);  reg2 = bigcdr(reg2); }
          l1 = l2 = l;
          if(bigval(reg1)<bigval(reg2))
            { exchange; s1 = -s1; }
        }
      else if(l1<l2) { exchange; s1 = -s1; }
      do { if(l2<l1) { h = bigval(reg1); l2++; }
           else { h = bigval(reg1) - bigval(reg2);
                  reg2 = bigcdr(reg2); }
           if(h == 0)  k++;
           else
             { if(h<0){ h += BASE; e--; d = BM1; }
                else d = 0L;
               laydown;
               e = h; }
         } while((reg1 = bigcdr(reg1)) != NIL);
       d = 0;
    }  /* end of else in case of s<0 */
  laydown;
  if(l == 0) reg1 = zinteger(bigval(dummybig.Xcdr)*s1);
  else reg1 = zbig((long)((l+1)*s1), dummybig.Xcdr);
  dummybig.Xcdr = NIL;  /* clear memory */
}
#undef addon
#undef laydown
#undef exchange

#define addon(Q)  if(Q>0) z=bigcdr(z)=zbig(Q,NIL); \
   else if(z==Sexp(&dummybig))l--; else  z=bigcdr(z)=zbig(Q,NIL)


void zdivision(p)           /* arbitrary precision integer division */
int p;                                  /* p == 0  ===> remainder */
{ long a,b,rl,s,q,v,u,*np;              /* p == 1  ===> quotient  */
  int l1,l2,l,i,j,k;                    /* p == 2  ===> divide    */
  PSEXP z;
  longdbl e,d;

  if(fixp(reg1))
    { a = intval(reg1);
      if(fixp(reg2))
        { b = intval(reg2);
          if(p != 1)
            { reg1 = zinteger(a % b);
              if(p == 2) { reg2 = zinteger(a / b); Xcons(); }
              return; }
          reg1 = zinteger(a / b);
          return; }
      else                /* division of small # by big # */
        { if(p == 0) return;
          if(p == 2) reg2 = reg1;
          reg1 = Sexp(&ZERO);
          if(p == 1) return;
          Cons();
          return; }
    }
  else
    { if((a = bigval(reg1))<0)    /* first argument is big */
        { l1 = - (int)a;
          s = -1;}
      else { l1 = (int)a; s = 1; }
      reg1 = bigcdr(reg1);
    }
  z = Sexp(&dummybig);
  if(fixp(reg2))    /* first argument is big */
    { b = intval(reg2);
      if(b<0) s = -s;
      b = labs(b);   l = l1;
      v = 0;                    /*remainder*/
      do { rl = bigval(reg1);
           v = udivide(v,&rl,b);
           if(p>0) addon(rl);
           reg1 = bigcdr(reg1);
         } while (reg1 != NIL);
      if(p!=1)
        { reg1 = zinteger(v);
          if(p) reg2 = reg1; else return; }
    }
  else                  /* both arguments are big */
    { b = bigval(reg2);
      if(b<0) s = -s;
      l2 = abs((int)b);
      l = l1 -l2 + 1;
      if(arit2sz<=l2+2) resize(&arit2, &arit2sz,l2+2);
      np = arit2 + l2;
      for(i=1; i<=l2; i++)
        { reg2 = bigcdr(reg2);
          *np-- = bigval(reg2); }
      np = arit1 + l1;
      if(arit1sz<=l1+2) resize(&arit1, &arit1sz,l1+2);
      arit1[0] = arit1[l1+1] = arit2[0] = 0;
      for(i=1; i<l1; i++)
        { *np-- = bigval(reg1);
          reg1 = bigcdr(reg1); }
      *np = bigval(reg1);
      d = arit2[l2-1] + longd(arit2[l2])*BASE;
      for(i = l1; i>=l2; i--)
        { e = (longd(arit1[i+1])*BASE)/d;
          q = (long)((longd(arit1[i-1]) + longd(arit1[i])*BASE)/d + e*BASE);
          u = 0;  k = i-l2;
          for(j=1;j<=l2; j++)
            { k++;
              e = longd(arit2[j])*q + u;
              u = (long) (e/BASE);
              v = arit1[k] - (long) (e - longd(u)*BASE);
              if(v<0) { u++; v+=BASE;}
              arit1[k] = v;
            }
          if(arit1[k+1]<u)
            { q--; k = i-l2; u = 0;
              for(j=1;j<=l2; j++)
                { k++;
                  v = arit1[k] + arit2[j] + u;
                  if(v<BASE) u = 0;
                   else { u = 1; v = v-BASE; }
                  arit1[k] = v;
                }
            }
          arit1[k+1] = 0;
          if(p>0) addon(q);
        }
      if(p!=1)                   /*get remainder in arith1 */
        { np = arit1;
          *np = 1; np += l2;
          while(*np-- == 0) l2--;
          np = arit1+1;
          if(l2>1)
            { reg1 = NIL;
              for(j=1; j<=l2; j++) reg1 = zbig(*np++,reg1);
              reg1 = zbig((long)l2,reg1); }
          else if(l2 == 1) reg1 = zinteger(arit1[1]);
          else reg1 = Sexp(&ZERO);
          if(p) reg2 = reg1; else return;
        }

    }
  if(z == Sexp(&dummybig)) reg1 = Sexp(&ZERO);
  else
    { reg1 = dummybig.Xcdr; dummybig.Xcdr = NIL;
      if(l>1) reg1 = zbig((long)s*l,reg1);
      else reg1 = zinteger(s*bigval(reg1)); }
  if(p == 2) Cons();
}   /* end of zdivision */

#undef addon
