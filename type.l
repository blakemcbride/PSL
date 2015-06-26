#define  reg1   registers[0]
#define  reg2   registers[1]
#define  reg3   registers[2]
#define  reg4   registers[3]
#define  reg5   registers[4]
#define  reg6   registers[5]
#define  reg7   registers[6]
#define  reg8   registers[7]
#define  reg9   registers[8]
#define  reg10  registers[9]
#define  reg11  registers[10]
#define  reg12  registers[11]
#define  reg13  registers[12]
#define  reg14  registers[13]
#define  reg15  registers[14]
#define  reg16  registers[15]

#define  M_ONE  small_num[0]
#define  ZERO   small_num[1]
#define  ONE    small_num[2]
#define  TWO    small_num[3]
#define  THREE  small_num[4]
#define  FOUR   small_num[5]
#define  FIVE   small_num[6]
#define  SIX    small_num[7]
#define  SEVEN  small_num[8]

#if DSTACK
#define saveregisters(n) memcpy(zstackptr-(n)+1,registers+1,(n)*sizeof(PSEXP))
#define restoreregisters(n) memcpy(registers+1,zstackptr-(n)+1,(n)*sizeof(PSEXP))
#else
#define saveregisters(n) memcpy(zstackp+zstackptr-(n)+1,registers+1,(n)*sizeof(PSEXP))
#define restoreregisters(n) memcpy(registers+1,zstackp+zstackptr-(n)+1,(n)*sizeof(PSEXP))
#endif

#define Spair(x)      ((PPAIR) (x))
#define Sid(x)        ((PID) (x))
#define Sstr(x)       ((PSTRING) (x))
#define Sfix(x)       ((PINTEGER) (x))
#define Sbig(x)       ((PBIG) (x))
#define Sfloat(x)     ((PFLOATING) (x))
#define Svector(x)    ((PVECTOR) (x))
#define Scode(x)      ((PFPOINTER) (x))
#define Serrmsg(x)    ((PERRMSG) (x))
#define Sforwardadr(x)((PFORWARDADR) (x))            /*v 6.0  *GU*/
#if IBMPC
#define Sstrelement(x)((PSTRELEMENT) (x))
#else
#define Sstrelement(x) ((char *) (x))
#endif



#define null(x)       ((x) == NIL)
#define type(x)       (Sid(x)->Xtype )
#define pairp(x)      (Sid(x)->Xtype == Tpair)
#define atom(x)       (Sid(x)->Xtype != Tpair)
#define idp(x)        (Sid(x)->Xtype == Tid)
#define stringp(x)    (Sid(x)->Xtype == Tstring)
#define fixp(x)       (Sid(x)->Xtype == Tinteger)
#define bigp(x)       (Sid(x)->Xtype == Tbig)
#define floatp(x)     (Sid(x)->Xtype == Tfloating)
#define vectorp(x)    (Sid(x)->Xtype == Tvector)
#define codep(x)      (Sid(x)->Xtype == Tfpointer)
#define errmsgp(x)    (Sid(x)->Xtype == Terrmsg)
#define forwarded(x)  (Sid(x)->Xtype == Tforwardadr)   /*v 6.0  *GU*/
#define numberp(x)    (fixp(x) || bigp(x) || floatp(x))
#define constantp(x)  (!idp(x) && !pairp(x))
#if BITF
#define inheap(x)     (Sid(x)-> Xisinheap)
#define globalp(x)    (Sid(x)-> Xisglobal)
#define fluidp(x)     (Sid(x)-> Xisfluid)
#define functionp(x)  (Sid(x)-> Xisfunction)
#define dclfluidp(x)  (Sid(x)-> Xisdclfluid)
#define inoblistp(x)  (Sid(x)-> Xisinoblist)
#else
#define attribute(x)  (Sid(x)-> Xattr)
#define inheap(x)     (attribute(x) & 0x20)
#define globalp(x)    (attribute(x) & 0x10)
#define fluidp(x)     (attribute(x) & 0x08)
#define functionp(x)  (attribute(x) & 0x04)
#define inoblistp(x)  (attribute(x) & 0x02)
#define dclfluidp(x)  (attribute(x) & 0x01)
#endif

#define floval(x)     (Sfloat(x)-> Xfloval)
#define hashlink(x)   (Sid(x)-> Xhashlink)
#define intval(x)     (Sfix(x)-> Xintval)
#define bigval(x)     (Sbig(x)-> Xintval)
#define bigcdr(x)     (Sbig(x)-> Xcdr)
#define pname(x)      (Sid(x)-> Xpname)
#define proplist(x)   (Sid(x)-> Xproplist)
#define value(x)      (Sid(x)-> Xvalue)
#if IBMPC
#define elmbody(x)    (Sstrelement(x) -> Xrealstr )   /*New in ver 3.3*/
#define elmfather(x)  (Sstrelement(x) -> Xbackpointer)/*New in ver 3.3*/
#define elmlength(x)  (Sstrelement(x) -> Xlength )    /*New in ver 3.3*/
#define father(x)     (strelement(x) -> Xbackpointer) /*New in ver 3.3*/
#define strbody(x)    (strelement(x) -> Xrealstr)     /*Changed in ver 3.3*/
#define strelement(x) (Sstr(x) -> Xstrbody)
#define strlength(x)  (strelement(x) -> Xlength)      /*New in ver 3.3*/
#else
#define strelement(x) (Sstr(x) -> Xstrelement)
#define strbody(x)    (strelement(x) + STROFFSET)
#define elmbody(x)    (((char *) (x)) + STROFFSET)
#define father(x)     (strelement(x) + sizeof(char))
#define elmfather(x)  (((char *) (x)) + sizeof(char))
#define storeelmfather(e,x)  ((dummystrptr=Sstr(x) ,  \
    movmem((char *) &dummystrptr,(char *)elmfather(e),sizeof(PSTRING))))
#define setfather(x)  (storeelmfather(strelement(x),x))
#define getelmfather(e)  \
   ((movmem((char *)elmfather(e),(char *) &dummystrptr,sizeof(PSTRING)) , \
     dummystrptr))
#define elmlength(x)  (*((char *) (x)))
#define strlength(x)  (*(strelement(x)))
#endif
#define upbv(x)       (Svector(x)-> Xupbv)
#define usedvec(x)    (Svector(x)-> Xused)
#define vectelt(x,n)  (vectelts(x)[n])
#define vectelts(x)   (Svector(x)-> Xvectelts)
#define forwardadr(x) (Sforwardadr(x)->Xforwardaddress)   /*v 6.0  *GU*/
#define argno(x)      (Scode(x)-> Xargno)
#define fnc(x)        (*(Scode(x)-> Xfnc))
#define errorno(x)    (Serrmsg(x)-> Xerrorno)
#define errormsg(x)   (Serrmsg(x)-> Xerrormsg)
#define caadr(x)      (car(car(cdr(x))))
#define caar(x)       (car(car(x)))
#define cadar(x)      (car(cdr(car(x))))
#define cadddr(x)     (car(cdr(cdr(cdr(x)))))
#define caddr(x)      (car(cdr(cdr(x))))
#define cadr(x)       (car(cdr(x)))
#define car(x)        (Spair(x)-> Xcar)
#define cdadr(x)      (cdr(car(cdr(x))))
#define cdar(x)       (cdr(car(x)))
#define cddddr(x)     (cdr(cdr(cdr(cdr(x)))))
#define cdddr(x)      (cdr(cdr(cdr(x))))
#define cddr(x)       (cdr(cdr(x)))
#define cdr(x)        (Spair(x)-> Xcdr)

#if SLOWSTACK         /*alternative stack manipulation definitions*/
#if DSTACK
#define kpop(n)       kpops(n)
#define kset(n,x)     ksets(n,&(x))
#define kload(x,n)    kloads(&(x),n)
#define local(n)      (*(zstackptr - n))
#define local0        (*zstackptr)
#define kset0(x)      ksets(0,&(x))
#define kload0(x)     kloads(&(x),0)
#else
#define kpop(n)       (zstackptr -= n)
#define kset(n,x)     ksets(n,&(x))
#define kload(x,n)    kloads(&(x),n)
#define local(n)      (zstackp[zstackptr - n])
#define local0        (zstackp[zstackptr])
#define kset0(x)      ksets(0,&(x))
#define kload0(x)     kloads(&(x),0)
#define STACKSIZE     1500            /*size of stack in static case*/
#endif
#else
#if DSTACK
#define kpop(n)       (zstackptr -= n)
#define kset(n,x)     (*(zstackptr - n) = x)
#define kload(x,n)    (x = *(zstackptr - n))
#define local(n)      (*(zstackptr - n))
#define local0        (*zstackptr)
#define kset0(x)      (*zstackptr = x)
#define kload0(x)     (x = *zstackptr)
#else
#define kpop(n)       (zstackptr -= n)
#define kset(n,x)     (zstackp[zstackptr - n] = x)
#define kload(x,n)    (x = zstackp[zstackptr - n])
#define local(n)      (zstackp[zstackptr - n])
#define local0        (zstackp[zstackptr])
#define kset0(x)      (zstackp[zstackptr] = x)
#define kload0(x)     (x = zstackp[zstackptr])
#define STACKSIZE     1500            /*size of stack in static case*/
#endif
#endif

#define zreturn(x)    { reg1 = (x); return; }
#define zkreturn(x,n) { reg1 = (x); kpop(n); return; }

#define ch(x)   (Sexp(&chrid[(x)]))

struct alistentry  {PSEXP alistid;
                    PSEXP alistval; } ;

typedef struct alistentry ALISTENT, *PALISTENT;

#define NTYPES    7
#if IBMPC
#define STROFFSET (sizeof(struct Xstrelmnt)-254)
#else
#define STROFFSET (sizeof(char)+sizeof(PSTRING))
#endif
#define STROFFSET2 (STROFFSET + 1)



struct page { struct page *nextpage;
              PSEXP free;
              char pagebd[PAGESIZE]; } ;

typedef struct page PAGE, *PPAGE;
#if DSTACK
struct errortrap { jmp_buf errsave;
                   PPSEXP stacksave;
                   PALISTENT curalist;
                   unsigned msgprint : 1;
                   unsigned backtrace : 1; } ;
#else
struct errortrap { jmp_buf errsave;
                   int stacksave;
                   PALISTENT curalist;
                   unsigned msgprint : 1;
                   unsigned backtrace : 1; } ;
#endif

typedef struct errortrap ERRORTRAP;

#define putmark(x)  (type(x) |= 0x80)
#define clrmark(x)  (type(x) &= 0x7f)
#define marked(x)   (type(x) &  0x80)
#define gtype(x)    (type(x) &  0x7f)

#define cnewline currlin++; \
                 if (pageln && currlin >= pageln) \
                    {putc('\f',outputf); currlin = 0; }

#if _TURBOC_
#define longd(x)  ((long double)(x))
#define longdbl   long double
#undef abs
#else
#define longd(x)  ((double)(x))
#define longdbl   double
#endif


#ifndef labs
#define labs(x) ((x)<0?-(x):(x))
#endif
#ifndef fabs
#define fabs(x) ((x)<0.0?-(x):(x))
#endif   

#ifndef min
#define min(a,b)        (((a) < (b)) ? (a) : (b))
#endif
