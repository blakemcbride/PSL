FILE *inputf = stdin;
FILE *outputf = stdout;

char tx[TXSIZE];
char yytext[128];

int traceable = 0;            /*trace apply */
unsigned currcol = 0;
unsigned lineln = 72;
unsigned currlin = 0;
unsigned pageln = 0;
unsigned delimflag;
unsigned digitcnt = 0;       /* # of digits of parsed integer. */
long *arit1;                 /* pointer to arith array. used by big arith */
long *arit2;                 /* routines */
unsigned arit1sz = 100;      /* sizes of these arrays */
unsigned arit2sz = 100;
unsigned gensymcounter = 0;
unsigned gcpage;               /*used in gc. collection   T.Y. */
unsigned chrcount = 0;         /*counts # of characters in p-name space */
unsigned strcount = 0;         /*counts # of characters in string space */
unsigned atompgc;        /*GC on atom's print name space*/
unsigned strgc;          /*GC on string space*/
#if DSTACK
int STACKSIZE = 1024;      /*local stack size*/
#endif
unsigned ALISTLENGTH = 512;    /*alist stack length*/
unsigned PNSLENGTH = 4000;     /*atom's print name space*/
unsigned STRLENGTH = 3500;     /*string space length*/

unsigned maxpair = 1440;       /*max empty work space if possible*/
unsigned maxpage = 5;          /*max pages allocated without GC */

PAIR dummypair = {Tpair, NIL, NIL };
BIG dummybig = {Tbig, 0L, NIL};
#if ! IBMPC
PSTRING dummystrptr;
#endif

INTEGER small_num[9] = {
      {Tinteger, -1L},
      {Tinteger, 0L},     /* mostly used numbers TY*/
      {Tinteger, 1L},
      {Tinteger, 2L},
      {Tinteger, 3L},
      {Tinteger, 4L},
      {Tinteger, 5L},
      {Tinteger, 6L},
      {Tinteger, 7L} };

ERRMSG forprog = { Terrmsg, 0, NIL };

PALISTENT alist;
PALISTENT alisttop,zalisttop;
PPAGE pages[NTYPES], cpages[NTYPES];
PPAGE freepages;

unsigned sz[NTYPES] = { sizeof(PAIR),
                        sizeof(ID),
                        sizeof(STRING),
                        sizeof(INTEGER),
                        sizeof(BIG),
                        sizeof(FLOATING),
                        sizeof(VECTOR) };

unsigned gcfree[NTYPES];
unsigned npages;
unsigned tnpages[NTYPES];
PCHAR pnmchp, strchp, startpns, startstr;

#if DSTACK
PPSEXP zstackp,zstackptr;
#else
PSEXP zstackp[STACKSIZE];
int zstackptr = -1;
#endif

#if SLOWSTACK
#if DSTACK
void kpops();
#endif
void ksets();
void kloads();
#endif

ERRORTRAP trap[30];
ERRORTRAP *curtrap = trap;
char *curpos;
PSEXP registers[16];
char *environment;

