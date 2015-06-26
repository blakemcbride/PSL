#if LATTICE
#include "\red\lisp\env.c"
#endif

char last_gc=0;             /*reason of last garbage collection*/
int trace1=0,trace2=0;      /*trace level counters */
extern unsigned NOFPAIR;
#define movmem(s,t,n)   memcpy(t,s,n)       /* in case of undefined movmem */
                             /* patch 1 */
#if DSTACK
PPSEXP zstacktop;

void kalloc(n)
unsigned n;
{ zstackptr += n;        /*zstackptr is pointer TY*/
  if(zstackptr > zstacktop) zerror(37);       /*TY*/
}
#else
void kalloc(n)
unsigned n;
{ zstackptr += n;                 /*zstackptr is integer TY*/
  if(zstackptr > STACKSIZE-1) zerror(37);              /*TY*/
}
#endif
                   /*Slows down the lisp but generates (in general) */
#if SLOWSTACK      /*smaller code.*/
#if DSTACK         /*dynamic procedural stack manipulation TY*/
void  ksets(n,x)
  unsigned n;
  PPSEXP x;
  { *(zstackptr - n) = *x; }

void kloads(x,n)        /* usage:   kload(&reg1,0); */
  unsigned n;
  PPSEXP x;
  { *x = *(zstackptr - n); }

void kpops(n)
  unsigned n;
  { zstackptr -= n; }

#else              /*static procedural stack manipulation TY*/
void ksets(n,x)
  unsigned n;
  PPSEXP x;
  { zstackp[zstackptr - n] = *x; }

void kloads(x,n)        /* usage:   kload(&reg1,0); */
  unsigned n;
  PPSEXP x;
  { *x = zstackp[zstackptr - n]; }

#endif
#endif

void zalist(x, y)
PSEXP x, y;
{
#if BITF
  if(globalp(x) || functionp(x)) zerror(2, x);
#else
  if(attribute(x) & 0x14) zerror(2, x);
#endif
  if(alisttop > zalisttop) zerror(38);          /*TY*/
  alisttop ->alistid = x;
  alisttop++->alistval = y;
  return;
}

void zbind(x, y)    /*GU*/  /*This is new*/
PSEXP x, y;
{
  zalist(x,value(x));
  value(x) = y;
}

void zunbind(n)     /*GU*/  /*This is also new*/
unsigned n;
{
  while (n)
   { alisttop--;
     value(alisttop->alistid) = alisttop->alistval;
     n--;}
}

PSEXP zalloc(tp)
char tp;
{ int n=0;
  PPAGE zgetpage();
  PSEXP v;
  PPAGE p;

  while(1)
    { p = cpages[tp];
      while(1)
        { if((v = p->free) != NULL)
            { p->free = forwardadr(v);
              return((PSEXP)v); }
          if(p->nextpage == NULL) break;
          cpages[tp] = p = p->nextpage;
        }                    /*undefined expansion is suppressed below TY*/
      switch(tp) {
         case Tpair :
           if((tnpages[Tpair] < maxpage) || (gcfree[Tpair] < maxpair))
              goto no_gc;
           break;
         case Tid :
         case Tinteger :
         case Tbig :
         case Tfloating :
           if((last_gc == tp) || (gcfree[tp] < 3*PAGESIZE/sz[tp]))
               goto no_gc;
           break;
         default :
           if(gcfree[tp] < PAGESIZE/sz[tp]) goto no_gc;
           break;
        }
gc:
      if(n) zgarbage(Tforwardadr);              /*compactifying GC.  TY*/
        else zgarbage(tp);
      if(gcfree[tp] != 0) continue;
      n++;
no_gc:
      cpages[tp] = p->nextpage = zgetpage(tp);
      if(p->nextpage == NULL)
        if(n<2) goto gc;
          else zerror(35);           /*not enough memory TY*/
    }
}

PCHAR zcalloc(n)
unsigned n;
{
  PCHAR v;

  if((chrcount + n) >= PNSLENGTH)  /* new in ver 3.3  ... GU */
    { zgarbage(Tpname);
      if ((chrcount + n) > PNSLENGTH)  zerror(32); }
  chrcount += n;
  v = pnmchp;
  pnmchp += n;
  return(v);
}

PCHAR zsalloc(n)             /* Brand new in ver 3.3  ... GU */
unsigned n;
    /*Actual # of char's to build the STDLisp-string, e.g. for "ali" n=5 */
{
  PCHAR v;

  if((strcount + n + STROFFSET2) >= STRLENGTH)
    { zgarbage(Tsname);
      if ((strcount + n + STROFFSET2) > STRLENGTH)  zerror(33); }
  v = strchp;
  elmlength(v) = (char) n; /* here we store the length immediately */
  n += STROFFSET2;
    /* [sizeof(Xlength) + sizeof(PSTRING):backpointer + 1 byte for:'\0'] */
  strcount += n;
  strchp += n;
  return(v);
}

void zedit(x)
char *x;
{
  if((currcol += strlen(x)) > lineln)
    { putc('\n', outputf);
      currcol = strlen(x); }
  fprintf(outputf, "%s", x);
  return;
}

void pr_reg1(int col)
{ if(reg1 == NULL) fprintf(outputf, "NULL\n");
    else
     { currcol = col;
       Prin1(); }
  Terpri();
}

void dispargs(i)
unsigned i;
{ unsigned j;
  PSEXP x;

  if(i==0 || i>16) return;
  x = reg1;
  for(j = 0; j < i; j++)
    { reg1 = registers[j];
      if(!null(reg1))
          { fprintf(outputf,"reg%-2d : ",j+1);
            pr_reg1(9);
          }
    }
  reg1 = x;
}

void dispstackarg()
{ unsigned j;
  PALISTENT u;
  PSEXP x;
#if DSTACK
  PPSEXP v;
#else
  int i;
#endif

  fprintf(outputf, "\n");
  x = reg1;
  dispargs(16);
#if DSTACK
  v = zstackptr;
  for(j = 0; j <= 15; j++)
    { if(v < zstackp) goto l1;
      reg1 = *v;
      if(!null(reg1))
        { fprintf(outputf,"STACK %4d-> ",j);
          pr_reg1(14);
        }
      v--; }
#else
  i = zstackptr;
  for(j = 0; j <= 15; j++)
    { if(i < 0) goto l1;
      reg1 = zstackp[i];
      if(!null(reg1))
        { fprintf(outputf,"STACK %4d-> ",i);
          pr_reg1(14);
        }
      i--; }
#endif
l1:
  u = alisttop-1;
  for(j = 0; j <= 15; j++)
    { if(u < alist) break;
      fprintf(outputf,"Alist top %2d-> ",j);
      currcol = 16;
      fprintf(outputf,"(");
      reg1 = u->alistid;
      Prin1();
      reg1 = u->alistval;
      if(reg1 == NULL) fprintf(outputf," . NULL");
        else
          { zedit(" . ");
            Prin1(); }
       zedit(")");
       Terpri();
       u--;
     }
  reg1 = x;
}                  /*end of stack argument dump procedure*/

void zerror (n, a1, a2, a3)
unsigned n;
PSEXP a1, a2, a3;
{
  PSEXP a[3];
  unsigned i = 0;
  char *m;
  PALISTENT als;
  PSEXP x;

  x = reg1;
  if(currcol)
    { putc('\n', outputf);
      cnewline;
      currcol = 0;
    }

  if(curtrap->msgprint)
    { a[0] = a1;
      a[1] = a2;
      a[2] = a3;
      m = emessages[n];
      fprintf(outputf, "***** ");
      currcol = 6;
      while(*m)
        if(*m == '%')
          { reg1 = a[i++];                /*GU*/
            Prin1();                      /*GU*/
            m++;
          }
        else
          { putc(*(m++), outputf);
            currcol++;
          }
      putc('\n', outputf);
      cnewline;
      currcol = 0;
    }
  reg1 = x;
  if(curtrap->backtrace) dispstackarg();
  als = curtrap->curalist;
  while(als < alisttop) zpopalist();
  zstackptr = curtrap->stacksave;
  longjmp(curtrap->errsave, n+1);
  trace2=trace1 = 0;
  return;
}

void zescape(u)
PSEXP u;
{
  char *x, *y;

  switch(type(u)) {
    case Tid : x = pname(u);
               y = tx;
               if(!isalpha(*x)) *y++ = '!';
               *y++ = *x++;
               while(*x)
                 { if(!isalpha(*x) && !isdigit(*x)) *y++ = '!';
                   *y++ = *x++;
                 }
               *y = 0;
               return;
    case Tstring : x = strbody(u);
                   y = tx;
                   *y++ = '"';
                   while(*x)
                     { if(*x == '"') *y++ = '"';
                       *y++ = *x++;
                     }
                   *y++ = '"';
                   *y = 0;
                   return;
    case Tfloating : sprintf(tx, "%lg", floval(u));
                     x = tx;
                     while(*x)
                       if(*x++ == '.') return;
                     y = tx;
                     while(*y)
                       { if(*y == 'e') break;
                         y++;
                       }
                     if(*y)
                       { while(x >= y)
                           { *(x+1) = *x;
                             x--;
                           }
                         *y = '.';
                         return;
                       }
                     *y++ = '.';
                     *y = 0;
                     return;
    case Tinteger : sprintf(tx, "%ld", intval(u));
                    return;
    case Tfpointer : sprintf(tx, "#%lo#", (long) (Scode(u)->Xfnc));
                     return;
  }
}


PPAGE zgetpage(tp)  /* modified to get allways from heap G.U.*/
unsigned tp;
{
  unsigned n,s1;                                                                           /*TY*/
  char *x, *y, *z;
  PPAGE p;

  if ((p = freepages) != NULL) freepages = p->nextpage;
  else
    { p = (PPAGE) myalloc(sizeof(PAGE));
      if(p == NULL) return(NULL);
      npages++; }
  tnpages[tp]++;
  gcfree[tp] += (int) PAGESIZE/sz[tp];
  x = NULL;
  s1 = sz[tp];
  z = p->pagebd;
  for(n=0; n<PAGESIZE; n+=s1)
    { y = z + n;
      if(tp == Tvector) usedvec(y) = 0;    /*ver 5.2 TY*/
      type(y) = tp;
      forwardadr(y) = (PSEXP) x;  /* new link in GC collection 6.0 TY */
      x = y; }
  p->free = (PSEXP) x;
  p->nextpage = NULL;
  return(p);
}

void zinitpages()
{
  PPAGE zgetpage();
  unsigned n;

  npages = 0;
  freepages = NULL;
  for(n=0; n<NTYPES; n++)
    pages[n] = cpages[n] = zgetpage(n);
}

void zurwelt()
{
  FILE *fp;
  unsigned int i;
  char *x;                                                           /*TY*/
  char u = 0;
  
  if (environment)
   { x = (PCHAR) getenv(environment);                                /*GU*/ 
     if (!x)
      printf("Initialization environment %s not set\n",environment); } /*GU*/
  else x = getenv("LISPINI");                                        /*GU*/
  if (!x) x = "LISP-INI";                                            /*GU*/
  fp = fopen(x,"r");
  if (!fp) 
    { printf("No initialization file: %s\n",x);
      if (ursize) zysuicide(6);
      else return; }
  inputf = fp;
  curtrap->msgprint = curtrap->backtrace = 1;
  curtrap->curalist = alisttop;
  curtrap->stacksave = zstackptr;
  initregs();
  setjmp(curtrap->errsave);
  if(u) zysuicide(4);
  Read1();
  if(!fixp(reg1)) zysuicide(2);
  if(ursize != (unsigned int) intval(reg1)) zysuicide(1);
  u = 1;
  if(ursize)
    { for (i=0; i<ursize; i++)
        { Read1();
          urwelt[i] = reg1; }
    }
  curtrap = trap;
  setjmp(curtrap->errsave);
  if(u>1) zysuicide(5);
  u++;
  Read1();
  if (!null(reg1)) zysuicide(3);
  Read1();
  while(!null(reg1)) { Eval(); Read1(); }
  fclose(fp);
}

void zysuicide(n)
unsigned n;
{
  switch (n)
   { 
     case 1 : printf("WRONG urwelt size!!\n"); break;
     case 2 : printf("Initialization file must begin with size of urwelt!\n"); 
              break;
     case 3 : printf("nil must end urwelt !\n"); break;
     case 4 : printf("WRONG urwelt!!\n"); break;
     case 5 : printf("error in eval part of urwelt!!\n"); break; 
     case 6 :
      printf("Need to load urwelt, but no initialization file exists!\n");
      break;
   }
 exit(n);
}

PSEXP zinteger(x)
long x;
{
  PSEXP v;

  if((x <= 7) && (x >= -1)) return(Sexp(&small_num[(int)x+1]));
  else
    { v = Sexp(zalloc(Tinteger));
      intval(v) = x;
      return(v);
    }
}

PSEXP zfloating(x)
double x;
{
  PSEXP v;                                                          /*TY*/

  v = Sexp(zalloc(Tfloating));
  floval(v) = x;
  return(v);
}

PSEXP zcons(x,y)
PSEXP x,y;
{ PPAIR v;
  v = Spair(zalloc(Tpair));
  car(v) = x;
  cdr(v) = y;
  return(Sexp(v));
}

PSEXP zcons1(x)
PSEXP x;
{ PPAIR v;
  v = Spair(zalloc(Tpair));
  car(v) = x;
  cdr(v) = NIL;
  return(Sexp(v));
}

void zconsc(x,y)
PSEXP x,y;
{
  reg1 = Sexp(zalloc(Tpair));
  car(reg1) = x;
  cdr(reg1) = y;
}

void zcons2c(x)
PSEXP x;
{ PPAIR v;
  v = Spair(zalloc(Tpair));
  car(v) = reg1;
  cdr(v) = x;
  reg1 = Sexp(v);
}

void zcons3c(x)
PSEXP x;
{ PPAIR v;
  v = Spair(zalloc(Tpair));
  car(v) = x;
  cdr(v) = reg1;
  reg1 = Sexp(v);
}

void zintern(n)
unsigned n;             /* n = 0 : set inoblistp to 0 */
{                       /* n = 1 : set inoblistp to 1 */
  PCHAR cp;             /* n = 2 : don't change inoblistp.. */
  unsigned hs;          /* .. if exist, otherwise create & set .. */
                        /* .. inoblistp to 1 */

  cp = tx;
  hs = 0;
  while(*cp)
    hs += *cp++;
  hs = hs%128;
  reg1 = Sexp(hashtab[hs]);
  while(reg1 != NULL)
    { if(strcmp(tx, pname(reg1)) == 0)     /* if found */
         {
#if BITF
           if(n!=2) inoblistp(reg1) = n;
#else
           if(n == 1) attribute(reg1) |= 0x02;
#endif
           return; }
      reg1 = Sexp(hashlink(reg1));
    }
  cp = zcalloc(strlen(tx)+1);
  reg1 = zalloc(Tid);
#if BITF
  globalp(reg1) = fluidp(reg1) = dclfluidp(reg1) = functionp(reg1) = 0;
  inheap(reg1) = 1;
  inoblistp(reg1) = !(!(n));  /* Trick n=1,2 :1 ; n=0 : 0 */
#else
  if(n == 0) attribute(reg1) = 0x20;                 /*it is in heap TY*/
    else attribute(reg1) = 0x22;
#endif
  hashlink(reg1) = hashtab[hs];
  value(reg1) = NULL;
  proplist(reg1) = NIL;
  pname(reg1) = cp;
  strcpy(cp, tx);
  hashtab[hs] = Sid(reg1);
}

void zpopalist()
{
  alisttop--;
  value(alisttop->alistid) = alisttop->alistval;
}

void zinterrupt()
{
/*curtrap = trap;        return to current trap not very beginning TY*/
  inputf = stdin;            /* change the direction of input */
  curtrap->backtrace = 0;    /* do not backtrace */
  zerror(29);
}

int ztracearg(u,v)                    /* traceable    trace     redefinition */
int u,v;                              /*     0          no          no       */
{ PSEXP x,y;                          /*     1          no          yes      */
  if(!traceable) return(0);           /*    >1          yes         yes      */
  if(traceable == 1) goto exit;
  trace2++;
  if(traceable < trace2) goto exit;
  x = reg1;
  if(traceable == 0x7fff) goto okey;
  y = reg2;
  reg1 = Sexp(&fname[u]);
  reg2 = Sexp(&trace);
  Flagp();
  reg2 = y;
  if(null(reg1)) { reg1 = x; goto exit; }
okey:
  trace1++;
  zedit(">>Entering  ");
  fprintf(outputf,"%3d",trace1);
  zedit(" : ");
  zedit(pname(Sexp(&fname[u])));
  Terpri();
  reg1 = x;
  if(v) dispargs(v);
exit:
  return(zredefined(u,v));
}

int zredefined(u,v)
int u,v;
{ PSEXP y;

  y = value(&fname[u]);
  if(car(y) == Sexp(&expr))
    { if(v > 5) zerror(40, Sexp(&fname[u]));
      switch(v)
       { case 0 : break;
         case 1 : Ncons(); break;
         case 2 : List2(); break;
         case 3 : List3(); break;
         case 4 : List4(); break;
         case 5 : List5(); break; }
      reg2 = reg1;
      reg1 = cdr(y);
      Apply();
      return(1);}
  else if(car(y) == Sexp(&fexpr))
    { if(v != 1) zerror(40, Sexp(&fname[u]));
      reg2 = reg1;
      reg1 = cdr(y);
      Apply();
      return(1); }
  else return(0);
}

void zundefined(u,v)
int u,v;
{ PSEXP x;
  x = value(&fname[u]);
  if(null(x)) return;
  if(car(x) != Sexp(&expr)) zerror(34, Sexp(&fname[u]));
  zredefined(u,v);
}

void ztraceval(u)
int u;
{ PSEXP x,y;
  if(traceable < 2) return;
  trace2--;
  if(traceable <= trace2) return;
  x = reg1;
  if(traceable == 0x7fff) goto okey;
  y = reg2;
  reg1 = Sexp(&fname[u]);
  reg2 = Sexp(&trace);
  Flagp();
  reg2 = y;
  if (null(reg1)) { reg1 = x; return; }
okey:
  zedit(">>Leaving ");
  fprintf(outputf,"%3d",trace1);
  zedit(" : ");
  trace1--;
  zedit(pname(Sexp(&fname[u])));
  Terpri();
  reg1 = x;
  zedit("With value : ");
  Print();
}

void errorreturn()
{ printf("\nNOT ENOUGH MEMORY\n");
  exit(1);
}

/* Garbage Collection part */

void clear_forward(char tp)
{ unsigned s,i;     /* clear forward type from partially filled page TY */
  PPAGE pg;
  PCHAR p;

  pg = pages[tp];
  if(pg->nextpage == NULL) return;        /* is this the only page */
  s = sz[tp];
  i = PAGESIZE/s;
  while(pg!=NULL)                         /* find partially filled page */
    { if(pg->free != NULL) break;
      pg = pg->nextpage; }
  p = pg->pagebd;
  while(i--)                            /* scan the page */
    {if(forwarded(p)) type(p) = tp;
     p += s; }                          /* take next ID */
}


void zmark(x)
PSEXP x;
{ PCHAR p;
 if(x == NULL) return;              /*it is necessary TY*/
 while(1)
   { switch(type(x)) {                   /* any cell marked or unmarked  TY*/
       case Tpair : putmark(x);
                    zmark(car(x));
                    x = cdr(x);
                    break;
       case Tbig  : putmark(x);
                    x = bigcdr(x);
                    break;
       case Tid :   putmark(x);
                    if(atompgc && inheap(x))          /*TY*/
                      { p = pname(x);
                        while (*p) p++;  /*for p-name space GC. G.U. ver 3.3*/
                        *p = '\1'; }
                    zmark(proplist(x));
                    zmark(value(x));
                    return;
       case Tvector : putmark(x);
                      { unsigned n;
                        for(n=0; n<=upbv(x); n++) zmark(vectelt(x, n));
                        return;
                      }
       case Tstring :              /*for string space GC. ...  ver 3.3*/
            if(strgc)              /*TY*/
              *(strbody(x)+strlength(x)) = '\1'; /*That's exactly where \0 is*/
       case Tinteger :
       case Tfloating : putmark(x);
       default: return;               /* marked cells TY */
     }
   }
}

int zcollect(p, tp, v)
PPAGE p;
int tp,v;
{
  unsigned n,s1,m=0;
  char *x, *y, *z;

  x = NULL;
  s1 = sz[tp];
  z = p->pagebd;
  for(n=0; n<PAGESIZE; n+=s1)
    { y = z + n;
      if(marked(y))
        { clrmark(y);
          v = 0;
        }
      else
       { if((tp == Tvector) && usedvec(y))
           { free(vectelts(v));   /*ver 5.2 TY*/
             usedvec(y) = 0; }
         forwardadr(y) = (PSEXP) x;           /* ver 6.0   TY*/
         x = y;
         m++;
       }
    }
  p->free = (PSEXP) x;
  if(v) { gcpage++; tnpages[tp]--; }
  else  gcfree[tp] += m;
  return(v);
}

void zcompactatom()     /* new in ver. 3.3 ... GU */
{
  PCHAR s, f, cp;
  unsigned int n;
  unsigned hs;
  PID x;

 for (s=pnmchp; s>=startpns; s--)  /* Blank out unreferenceds */
   switch  (*s)
     {case '\0' : while (*s != '\1'  &&  s >= startpns) *s-- = '\0';
      case '\1' : *s = '\0';}

 f = startpns;                   /* Find first emptied position */
 while (f<pnmchp)                /* From now on f will point to empty pos.*/
   if(!*f && !*++f) break;       /* Empty pos. is at least two */
   else f++;                             /* consecutive \0's */

 s = f;                        /* Nonsense to start the search at startpns */
 while (s<pnmchp)              /* s seeks to find the first movable stuff */
   if(!*s) s++;                /* movables are NOT \0 , so skip over any \0 */
   else                        /* strlen returns # of non-\0 */
     { n = strlen(s) + 1;      /* char`s in the string , so we add one */
       hs = 0;                 /* Now we'll find which atom owns this pname */
       cp = s;
       while(*cp) hs += *cp++;
       hs = hs%128;            /* That's our hash function */
       x = hashtab[hs];        /* Fetch the hash bucket */
       while(x != NULL)        /* Search the bucket for.. */
         { if(strcmp(s, pname(x)) == 0)   /* does the p-name match ? */
            { pname(x) = f;  /* match found, so pname points to the new pos */
              break; }         /* Quit search */
           x = hashlink(x);} /*No match;carry on with another atom in bucket*/
                          /* After all works we may throw away the IF below */
       if (x == NULL)
          printf("\n %s cannot find father in p-name compaction!\n",s);
       movmem(s,f,n);          /* Move all the stuff back to the empty pos */
       s += n;                 /* Advance both pointers */
       f += n; }
 pnmchp = f;                   /* The difference of pnmchp and f is the : */
                               /* "KISA GUNUN KARI" */
 chrcount = pnmchp - startpns;
}

void zcompactstring()     /* new in ver. 3.3 ... GU */
{
char  *s, *p;

 s = startstr;  /* Start search of first-free-pos. at startstr */
 while (s<strchp)
  { p = s + *s + STROFFSET;
    if (!*p) break;
    *p = '\0';
    s = p + 1;}

 p++;
 while (p<strchp)
   { if (!*(p + *p + STROFFSET))
     { p += *p + STROFFSET2;
       continue;}
     movmem(p,s,(unsigned) (*p+STROFFSET));
#if IBMPC
     elmfather(s) -> Xstrbody = Sstrelement(s);  /*Update the owner*/
#else
     getelmfather(s) -> Xstrelement = Sstrelement(s);
#endif
     *(s + *s + STROFFSET) = '\0';
     p += *s + STROFFSET2;
     s += *s + STROFFSET2; }
 strchp = s;
 strcount = strchp - startstr;
}

void zrelocate(tp)
int tp;
{ PCHAR fre, active;
  PPAGE p, freepage, activepage; /* freepage points to page that contains */
  unsigned nf=0,na;              /* the element fre is pointing to */
  unsigned c,s;

  s = sz[tp];         /* get the size of that type: (tp) */
  c = PAGESIZE/s - 1; /* count of type (tp) that lives in a single page */
  activepage = NULL;
  p = freepage = pages[tp];/* seeking for free pos. starts from head of link */
  while(p!=NULL)      /* Form a back pointer chain over the free subfield */
    { p->free = (PSEXP) activepage;
      activepage = p;
      p = p->nextpage; }
                          /* here activepage contains ptr to last page */
  if(freepage == activepage) return;  /* no need to relocation  TY */

  fre = freepage->pagebd;       /* first element in the first page */
  nf = 0;                       /* we count such that the first is 0 */

  active = activepage->pagebd+PAGESIZE-s; /* last element in the last page */
  na = c;

testfree:          /* searching down for the first empty element position */

  if (!marked(fre)) goto testactive;  /* found an empty pos....HURRA */

seeknew:               /* carrying on the down search  */

  if (nf<c)
    { fre += s;    /* advance to following element position */
      nf++; }
  else              /* that means we're at the end.. get next page */
    { freepage->free = NULL; /*clear the back pointer. mark full page TY*/
      freepage = freepage->nextpage;
      if(freepage == activepage) return;  /* end of relocation phase TY */
      fre = freepage->pagebd;       /*pointing to first element in page */
      nf = 0; }
  goto testfree;       /* is this position available ??? */

testactive:    /* searching up for the first marked  element */

  if (marked(active)) /* found some thing to move up .. let's move it */
    {         /*no danger in using memcpy, source&target cannot overlap */
     memcpy(fre,active,s);
     type(active) = Tforwardadr;    /* We leave a forwarding address */
     forwardadr(active) = Sexp(fre);    /* That is where we moved to */
     goto seeknew; }        /* try to find another candidate to move */

  if (na)
    { active -= s;    /* go up in page to the previous element position */
      na--; }
  else       /* that means we're at the start ..get next page to search */
    { activepage = (PPAGE) (activepage->free);
      if(freepage == activepage) return;  /* end of relocation phase TY */
        /* now we set active to point to the last element in page*/
      active = activepage->pagebd + PAGESIZE - s;
      na = c; }    /* we were counting starting with 0 */

  goto testactive;       /* was this element marked ??? */
}

void zgarbage(tp) /* .... COMPACTIFYING GARBAGE COLLECTION .... *GU*/
char tp;          /* To turn-on compactification do (setq !*gctest 2) */
{                  /* all the same until the end of the marking phase */
  PCHAR p;                                        /*v 6.0  *GU*/
  unsigned s,k,m,r;                               /*v 6.0  *GU*/
  int info,n;
  unsigned i,j;
  PPSEXP x;
  PALISTENT al;
  PID ob,obl;
  PPAGE pgx,pg;
  PPAIR fn;

   /*   dispstackarg(); GU */
  last_gc = tp;
  i = chrcount;                                               /*TY*/
  j = strcount;                                               /*TY*/
  if(chrcount > PNSLENGTH-512) atompgc = 1; else atompgc = 0; /*TY*/
  if(strcount > STRLENGTH-512) strgc = 1; else strgc = 0;     /*TY*/
  if(fixp(gcflag.Xvalue)) info = (int)intval(gcflag.Xvalue);
    else info = gcflag.Xvalue != NIL || tp == Tuser;
  if(info) printf("\nGarbage collection for %d ...",tp);
  for(n=0; n<NTYPES; n++) gcfree[n] = 0;
  gcpage = 0;

/*******MARKING PHASE *******/

  al = alisttop;
  while(al-- != alist)
    { zmark(al->alistid);
      zmark(al->alistval);
    }
  /* let's mark from stack ... hahaha finally I DID IT */
#if DSTACK
  x = zstackp;    /*dynamic stack garbage collection TY*/
  while(x <= zstackptr) zmark(*x++);
#else
  for(n=0; n<=zstackptr; n++) zmark(zstackp[n]);
#endif
  x = registers;         /* mark registers */
  for(n=0; n<16; n++) zmark(*x++);
  zmark(bigcdr(&dummybig));
  /* Now mark from urwelt */           /*GU*/
  for (n=0; n<ursize; n++) zmark(urwelt[n]);
  for(n=0; n<128; n++)
    { ob = hashtab[n];     /*new marking phase of oblist. ver. 5.2 TY */
      while(ob != NULL)
        { if(value(ob) != NULL || !null(proplist(ob))) zmark(Sexp(ob));
          ob = hashlink(ob);
        }
    }

/******* SWEEP AND UNMARKING PHASE *******/

  for(n=0; n<128; n++)
    { if((ob = hashtab[n]) == NULL) continue;
l1:   if(inheap(ob)) { if(!marked(ob)) { ob = hashlink(ob); goto l1; }}
	else if(ob) clrmark(ob);  /* if(ob) is new.  7.0 GU */
      hashtab[n] = ob;                 /*now different inheap 5.2 TY*/
      if (ob) while((obl = hashlink(ob)) != NULL)  /* if(ob) is new 7.0 GU */
	if(!inheap(obl)) { clrmark(obl);  ob = obl; }
	  else if(marked(obl)) ob = obl;
	  else hashlink(ob) = hashlink(obl);
    }
  fn = (PPAIR) fnvlpr;           /*pointer to function value pairs*/
  for(n=0; n<NOFPAIR; n++) clrmark(fn++);
  for(n=0; n<=8; n++) clrmark(&small_num[n]);
                                   /*clear the mark in static part TY*/
  if(atompgc)                                                    /*TY*/
    { if(info) printf("\nCompactify atom's p-name space ...");   /*TY*/
      zcompactatom(); }
  if(strgc)                                                      /*TY*/
    { if(info) printf("\nCompactify string space ...");          /*TY*/
      zcompactstring(); }
#if DEBUG
  if(info > 2) zdump("gc_dump1.lst");
#endif
/*  if(tp<Tforwardadr) goto skip_relocation; */
  if(info != 2) goto skip_relocation;

                   /*******   RELOCATION PHASE   *******/
  if(info) printf("COMPACTIFICATION has started...");   /*TY*/
  for(n=0; n<NTYPES; n++)  zrelocate(n);

                    /*******   UPDATING PHASE   *******/

                               /* updating ID space */
  for(n=0; n<128; n++)
    { if((ob = hashtab[n]) == NULL) continue;
      if(forwarded(ob)) ob = hashtab[n] = Sid(forwardadr(ob));
      while(1)
        { if(forwarded(proplist(ob))) proplist(ob)=Sexp(forwardadr(proplist(ob)));
          if((fn = Spair(value(ob))) != NULL && forwarded(fn))
               value(ob)=Sexp(forwardadr(fn));
          if((obl=hashlink(ob)) == NULL) break;
            else { if(forwarded(obl)) ob=hashlink(ob)=Sid(forwardadr(obl));
                     else ob = obl; }
        }
    }
                               /* updating PAIR space */
  pg = pages[Tpair];
  s = sz[Tpair];
  m = PAGESIZE/s;
  while(pg!=NULL)          /* scan the PAIR space linked via nextpage field */
    { p = pg->pagebd;
      r = m;
      while (r--)     /* scan the page */
        { if(marked(p))
            {if(forwarded(car(p))) car(p) = Sexp(forwardadr(car(p)));
             if(forwarded(cdr(p))) cdr(p) = Sexp(forwardadr(cdr(p))); }
          p += s; }      /* take next PAIR */
      if(pg->free != NULL) break;     /* last marked page TY */
      pg = pg->nextpage; }

                               /* updating STRING space */
  pg = pages[Tstring];
  s = sz[Tstring];
  m = PAGESIZE/s;
  while(pg!=NULL)       /* scan the STRING space linked via nextpage field */
    { p = pg->pagebd;
      r = m;
      while (r--)     /* scan the page */
        {
#if IBMPC
          father(p) = Sstr(p);   /* to check is much more complicated */
#else
          setfather(p);
#endif
          p += s; }      /* take next STRING */
      if(pg->free != NULL) break;     /* last marked page TY */
      pg = pg->nextpage; }

                               /* updating BIGNUM space */
  pg = pages[Tbig];
  s = sz[Tbig];
  m = PAGESIZE/s;
  while(pg!=NULL)      /* scan the BIGNUM space linked via nextpage field */
    { p = pg->pagebd;
      r = m;
      while (r--)     /* scan the page */
        { if(marked(p) && forwarded(bigcdr(p)))
             bigcdr(p) = Sexp(forwardadr(bigcdr(p)));
          p += s; }      /* take next BIGNUM */
      if(pg->free != NULL) break;     /* last marked page TY */
      pg = pg->nextpage; }

                               /* updating VECTOR space */

  pg = pages[Tvector];
  s = sz[Tvector];
  m = PAGESIZE/s;
  while(pg!=NULL)        /* scan the VECTOR space linked via nextpage field */
    { p = pg->pagebd;
      r = m;
      while (r--)     /* scan the page */
        { if(marked(p))
            for(k=0; k<=upbv(p); k++)
              if (forwarded(vectelt(p,k)))
                         vectelt(p,k) = Sexp(forwardadr(vectelt(p,k)));
          p += s; }      /* take next VECTOR */
      if(pg->free != NULL) break;     /* last marked page TY */
      pg = pg->nextpage; }

                               /* updating ALIST */
  al = alisttop;
  while(al-- != alist)
    { if (forwarded(al->alistid)) al->alistid=Sexp(forwardadr(al->alistid));
      if (forwarded(al->alistval)) al->alistval=Sexp(forwardadr(al->alistval));
    }

                               /* updating STACK */
#if DSTACK
  x = zstackp;    /*dynamic stack garbage collection TY*/
  while(x <= zstackptr)
    { if(forwarded(*x)) *x = Sexp(forwardadr(*x));
      x++; }
#else
  for(n=0; n<=zstackptr; n++)
     if (forwarded(zstackp[n])) zstackp[n] = Sexp(forwardadr(zstackp[n]));
#endif
                          /* updating REGISTERS */
  x = registers;
  for(n=0; n<16; n++)
    { if (forwarded(*x)) *x = Sexp(forwardadr(*x));
      x++; }

                          /* updating URWELT */
  for (n=0; n<ursize; n++)
    if (forwarded(urwelt[n])) urwelt[n] = Sexp(forwardadr(urwelt[n]));
#if DEBUG
  if (info > 2) zdump("gc_dump2.lst");
#endif
  for(n=0; n<NTYPES; n++) clear_forward(n);

        /****** END OF UPDATING PHASE OF COMPACTIFICATION ******/

        /*******  UNMARKING  *******/
skip_relocation:
  for(n=0; n<NTYPES; n++)
    { pg = pages[n];               /* it cannot be NULL TY */
      while(pg != NULL)
        if(zcollect(pg, n, pg->nextpage != NULL))
          { pgx = pg;
            pg = pg->nextpage;
            pgx->nextpage = freepages;
            freepages = pgx;
           }
        else break;
      pages[n] = pg;
      while(pg->nextpage != NULL)
        if(zcollect(pg->nextpage, n, 1))
          { pgx = pg->nextpage;
            pg->nextpage = pg->nextpage->nextpage;
            pgx->nextpage = freepages;
            freepages = pgx;
          }
        else pg = pg->nextpage;
    }
  for(n=0; n<NTYPES; n++)
     { if(pages[n] == NULL) pages[n] = zgetpage(n);
       cpages[n] = pages[n]; }
#if DEBUG
  if(info > 2) zdump("gc_dump3.lst");
#endif
  /*  dispstackarg(); GU */
  if(info)
   { printf("\n %d free PAGES have been created out of %d \n",gcpage,npages);
     printf("# of chr. in ATOM's P-name space (before - after) GC: (%5d - %5d)\n",
             i,chrcount);
     printf("# of chr. in STRING space: (%5d - %5d)\n",j,strcount);
#if DSTACK
     n = zstackptr-zstackp;
     printf("STACK contains %5d pointers\n",n);
#else
     printf("STACK contains %5d pointers\n",zstackptr);
#endif
     n = alisttop - alist;
     printf("occupied ALIST entries %d\n",n);
     printf("        created      cells       number\n");
     printf("       free cells   per page    of pages\n");
     printf("PAIR   :%5d          %4d          %3d\n",
            gcfree[Tpair],PAGESIZE/sz[Tpair],tnpages[Tpair]);
     printf("ID     :%5d          %4d          %3d\n",
            gcfree[Tid],PAGESIZE/sz[Tid],tnpages[Tid]);
     printf("STRING :%5d          %4d          %3d\n",
            gcfree[Tstring],PAGESIZE/sz[Tstring],tnpages[Tstring]);
     printf("INTEGER:%5d          %4d          %3d\n",
            gcfree[Tinteger],PAGESIZE/sz[Tinteger],tnpages[Tinteger]);
     printf("BIG    :%5d          %4d          %3d\n",
            gcfree[Tbig],PAGESIZE/sz[Tbig],tnpages[Tbig]);
     printf("FLOAT  :%5d          %4d          %3d\n",
            gcfree[Tfloating],PAGESIZE/sz[Tfloating],tnpages[Tfloating]);
     printf("VECTOR :%5d          %4d          %3d\n",
            gcfree[Tvector],PAGESIZE/sz[Tvector],tnpages[Tvector]);
   }
}                 /* end of garbage collection part */

void print_cell(FILE *df, PSEXP y)
{ if(y == NULL) { fprintf(df, " NULL\n"); return; }
  if(marked(y)) fprintf(df,"*");
    else fprintf(df," ");

  switch(gtype(y)) {
     case Tpair:
       fprintf(df,"%08lx  %02x  %08lx  %08lx\n", y,Tpair,car(y),cdr(y));
       return;
     case Tid:
#if BITF
       fprintf(df,"%08lx  %02x    %08lx  %08lx  %08lx  %08lx",
           y,Tid,hashlink(y),value(y), proplist(y),pname(y));
#else
       fprintf(df,"%08lx  %02x  %02x  %08lx  %08lx  %08lx  %08lx",
           y,Tid,0x00ff & attribute(y),hashlink(y),value(y),
               proplist(y),pname(y));
#endif
       if(strlen(pname(y))<15) fprintf(df,"  %s\n" ,pname(y));
          else fprintf(df,"\n");
       return;
     case Tstring:
       fprintf(df,"%08lx  %02x  %08lx\n",y, Tstring, Sstr(y));
       return;
     case Tinteger:
       fprintf(df,"%08lx  %02x  %12ld\n", y,Tinteger,intval(y));
       return;
     case Tbig:
       fprintf(df,"%08lx  %02x  %12ld  %08lx\n",
           y,Tbig,bigval(y),bigcdr(y));
       return;
     case Tfloating:
       fprintf(df,"%08lx  %02x  %20.14f\n",y, Tfloating, floval(y));
       return;
     case Tvector:
       fprintf(df,"%08lx  %02x  %5d %08x\n",y, Tvector, upbv(y), vectelts(y));
       return;
     case Tforwardadr:
       fprintf(df,"%08lx  %02x  %08lx\n",y, Tforwardadr, forwardadr(y));
       return;
     default:
       fprintf(df,"Unknown type: %02x  %08lx\n", 0x00ff & type(y),y);
    }
}

#if DEBUG

void zdump(fn)
char *fn;
{
  FILE *dumpf;
  unsigned i,n,s1;
  PCHAR z, y;
  PPAGE pg;
  PALISTENT al;
  PPSEXP x;

  dumpf = fopen(fn,"w");
  pg = pages[Tpair];
  i = 1;
  s1 = sz[Tpair];
  while(pg != NULL)
    { fprintf(dumpf,"\nPAIR SPACE:  page %d    pointer  %08lx\n",i,pg);
      fprintf(dumpf,"     free pair: %08lx\n", (pg->free));
      fprintf(dumpf,"   x   type(x)   car(x)   cdr(x)\n\n");
      z = pg->pagebd;
      for(n=0; n<PAGESIZE; n+=s1)
        { y = z + n;
          print_cell(dumpf,y); }
      i++;
      pg = pg->nextpage;
    }
  pg = pages[Tid];
  i = 1;
  s1 = sz[Tid];
  while(pg != NULL)
    { fprintf(dumpf,"\nID SPACE:  page %d    pointer  %08lx\n",i,pg);
      fprintf(dumpf,"     free id: %08lx\n", (pg->free));
      fprintf(dumpf,"  IDP    type  attr  hashlink   value   prop   pnamep  pname\n\n");
      z = pg->pagebd;
      for(n=0; n<PAGESIZE; n+=s1)
        { y = z + n;
          print_cell(dumpf,y); }
      i++;
      pg = pg->nextpage;
    }
  pg = pages[Tstring];
  i = 1;
  s1 = sz[Tstring];
  while(pg != NULL)
    { fprintf(dumpf,"\nString SPACE:  page %d    pointer  %08lx\n",i,pg);
      fprintf(dumpf,"     free id: %08lx\n", (pg->free));
      fprintf(dumpf,"  Str Ptr   type   string body pointer\n\n");
      z = pg->pagebd;
      for(n=0; n<PAGESIZE; n+=s1)
        { y = z + n;
          print_cell(dumpf,y); }
      i++;
      pg = pg->nextpage;
    }
  pg = pages[Tinteger];
  i = 1;
  s1 = sz[Tinteger];
  while(pg != NULL)
    { fprintf(dumpf,"\nInteger SPACE:  page %d    pointer  %08lx\n",i,pg);
      fprintf(dumpf,"     free int: %08lx\n", (pg->free));
      fprintf(dumpf,"    x  type(x) value of int x\n\n");
      z = pg->pagebd;
      for(n=0; n<PAGESIZE; n+=s1)
        { y = z + n;
          print_cell(dumpf,y); }
      i++;
      pg = pg->nextpage;
    }
  pg = pages[Tbig];
  i = 1;
  s1 = sz[Tbig];
  while(pg != NULL)
    { fprintf(dumpf,"\nBig Integer SPACE:  page %d    pointer  %08lx\n",i,pg);
      fprintf(dumpf,"     free big int: %08lx\n", (pg->free));
      fprintf(dumpf,"  x    type(x)  value_of_int(x)   cdr(x) \n\n");
      z = pg->pagebd;
      for(n=0; n<PAGESIZE; n+=s1)
        { y = z + n;
          print_cell(dumpf,y); }
      i++;
      pg = pg->nextpage;
    }
  pg = pages[Tfloating];
  i = 1;
  s1 = sz[Tfloating];
  while(pg != NULL)
    { fprintf(dumpf,"\nFloat SPACE:  page %d    pointer  %08lx\n",i,pg);
      fprintf(dumpf,"     free id: %08lx\n", (pg->free));
      fprintf(dumpf,"  Flt Ptr   type   floating value\n\n");
      z = pg->pagebd;
      for(n=0; n<PAGESIZE; n+=s1)
        { y = z + n;
          print_cell(dumpf,y); }
      i++;
      pg = pg->nextpage;
    }
  pg = pages[Tvector];
  i = 1;
  s1 = sz[Tvector];
  while(pg != NULL)
    { fprintf(dumpf,"\nVector SPACE:  page %d    pointer  %08lx\n",i,pg);
      fprintf(dumpf,"     free id: %08lx\n", (pg->free));
      fprintf(dumpf,"Vector Ptr   type  upbv   pointer\n\n");
      z = pg->pagebd;
      for(n=0; n<PAGESIZE; n+=s1)
        { y = z + n;
          print_cell(dumpf,y); }
      i++;
      pg = pg->nextpage;
    }
  al = alisttop;
  fprintf(dumpf,"\n  Alist space identifiers\n");
  fprintf(dumpf,"  IDP    type  attribute  hashlink  value  prop  pnamep  pname\n\n");
  while(al-- != alist) print_cell(dumpf,al->alistid);

  fprintf(dumpf,"\n  STACK\n");
#if DSTACK
  x = zstackp;    /*dynamic stack garbage collection TY*/
  while(x <= zstackptr) print_cell(dumpf,*x++);
#else
  for(n=0; n<=zstackptr; n++) print_cell(dumpf,zstackp[n]);
#endif
  fprintf(dumpf,"\n  REGISTERS\n");
  x = registers;
  for(n=0; n<16; n++) print_cell(dumpf,*x++);

  fprintf(dumpf,"\n  HASHLIST\n");
  for(n=0; n<128; n++) print_cell(dumpf,hashtab[n]);

  if(ursize)
    { fprintf(dumpf,"\n  URWELT\n");
      for (n=0; n<ursize; n++) print_cell(dumpf,urwelt[n]); }
  fclose(dumpf);
}

#endif

void initregs()
{ int n;
  PPSEXP x;

  trace2=trace1 = 0;
  x = registers;
  for(n=0; n<16; n++) *x++ = NIL;
}

void main(argc,argv)
int argc;
char *argv[];
{
  unsigned n;

  for(n = 1; n < argc; n++)
    { if((*argv[n]++) == '-')
        { *argv[n] = toupper(*argv[n]);
          if ((*argv[n])=='E')
           { argv[n]++;          /* skip over the 'E' */
             if (!*argv[n]) n++; /* so, user gave one blank */
             environment = (char *) malloc(strlen(argv[n]+1));
             strcpy(environment,argv[n]);
             continue; }
          sscanf((argv[n]+1),"%d",&gcpage);    /*converts string to integer*/
          switch(*argv[n])
           {
#if DSTACK
             case 'S' : STACKSIZE = gcpage; break;   /*DYNAMIC STACK*/
#endif
             case 'A' : ALISTLENGTH = gcpage; break;
             case 'P' : PNSLENGTH = gcpage; break;
             case 'T' : STRLENGTH = gcpage; break;
             case 'M' : maxpair = gcpage; break;
             case 'G' : maxpage = gcpage; break;
           }
        }
    }
#if DSTACK
  zstackp = (PPSEXP) calloc(STACKSIZE,sizeof(PSEXP));
  if(zstackp == NULL) errorreturn();   /*clearence of stack is necessary TY*/
  zstackptr = zstackp - 1;
  zstacktop = zstackp + STACKSIZE - 1;
#else
  for(n=0; n<STACKSIZE; n++) zstackp[n] = NULL;
#endif
  alist = (PALISTENT) myalloc(ALISTLENGTH*sizeof(ALISTENT));
  if(alist == NULL) errorreturn();
  alisttop = alist;
  zalisttop = alist + ALISTLENGTH - 1;
  pnmchp = (PCHAR) myalloc(PNSLENGTH);
  if(pnmchp == NULL) errorreturn();
  startpns = pnmchp;
  strchp = (PCHAR) myalloc(STRLENGTH);
  if(strchp == NULL) errorreturn();
  startstr = strchp;
  arit1 = (long *)myalloc(arit1sz*sizeof(long)); /* now allocate big arith arrays. */
  if(arit1 == NULL) errorreturn();
  arit2 = (long *)myalloc(arit1sz*sizeof(long));
  if(arit2 == NULL) errorreturn();
  for(n=0; n<NTYPES; n++) gcfree[n] = 0;
  if(maxpair == 1440)     /* initial value of maxpair is 1440 */
     maxpair = 5*PAGESIZE/sz[Tpair];
#if _TURBOC_
  if(maxpage == 5)     /* initial value of maxpage is 5 */
     maxpage = (coreleft()/sizeof(PAGE)) - 15;
  printf("\n %d pages pair space\n",maxpage);
  ctrlbrk(zinterrupt);
#endif
#if LATTICE
  if(maxpage == 5)
    maxpage = ((0xA0000l-(long)arit2)/sizeof(PAGE)) - 15;
  printf("\n %d pages pair space\n",maxpage);
  bdosx(0x25,(PCHAR) (zinterrupt) ,0x23);  /*Set Break handling interrupt*/
#endif
  zinitpages();
  initregs();
  inputf = stdin;
  outputf = stdout;
  yytext[0] = 0;
  curpos = "";
  zysignonoff(1);
  zurwelt();
  inputf = stdin;
  outputf = stdout;
  yytext[0] = 0;
  curpos = "";
  Standardfzlisp();
}
